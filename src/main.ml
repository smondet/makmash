open Nonstd
module String = Sosa.Native_string

(*
let current_slide = ref 0
type slide_kind = 
  | Markdown of string option * string
  | Title_slide of string * string * string
*)

let global_title = ref "No Title"
let global_images : string list ref = ref []

let markdown_to_html s =
  let check_comment s =
    eprintf "Checking comment %S\n%!" s;
    match String.split ~on:(`Character ' ') s
          |> List.map ~f:String.strip
          |> List.filter ~f:(fun s -> s <> "" && s <> "<!--" && s <> "-->")
    with
    | "itemize" :: more -> `Itemize more
    | other -> `None
  in
  let next_list_itermizes = ref None in
  let rec omd_to_html md =
    let override =
      let open Omd_representation in
      function
      | H1 h1 ->
        let new_slide =
          sprintf "\
          <section data-background=\"bg-front.png\" \
              data-background-size=\"100%% 100%%\" data-transition=\"zoom\">
              <div  style=\"text-align: left; left: 3em; color: #eee\">
                  <h2 style=\"color: #eee\" >%s</h2></div>"
            (omd_to_html h1)
        in
        global_title := Omd.to_text h1;
        Some new_slide
      | H3 h3 ->
        Some (sprintf "</section><section><h3>%s</h3>" (omd_to_html h3))
      | Html_comment c ->
        begin match check_comment c with
        | `Itemize args -> next_list_itermizes := Some args; Some ""
        | `None -> None
        end
      | Ul ul ->
        begin match !next_list_itermizes with
        | None  -> None
        | Some args ->
          let ulhtml =
            sprintf "<ul>%s</ul>"
              (List.map ul ~f:(fun omd ->
                   sprintf "<li class=\"fragment roll-in\">%s</li>"
                     (omd_to_html omd))
               |> String.concat ~sep:"\n")
          in
          next_list_itermizes := None;
          Some ulhtml
        end
      | Code_block (lang, code) ->
        let default () =
          let returned =
            sprintf "<pre class=\"terminal\"><code>%s</code></pre>" code in
          Some returned in
        begin match String.split ~on:(`Character ',') lang with
        | "commands" :: more ->
          let lines = String.split code ~on:(`Character '\n') in
          let subblock_class =
            if List.mem "itemize" more then "fragment" else "normal" in
          let spicy_lines =
            let close_previous = ref "" in
            List.map lines ~f:(fun line ->
                match line with
                | cmd when String.sub cmd 0 2 = Some " $" ->
                  sprintf "%s<code class=\"%s\">\
                           <span class=\"command\">%s</span>\
                           <span class=\"%s\">"
                    !close_previous
                    subblock_class
                    (omd_to_html [Text cmd])
                    subblock_class
                | other ->
                  close_previous :=  "</span></code>";
                  sprintf "%s"
                    (omd_to_html [Text other])
              )
          in
          let returned =
            sprintf "<pre class=\"terminal\">%s</span></code></pre>"
              (String.concat ~sep:"\n" spicy_lines) in
          Some returned
        | lang :: more ->
          begin try
            let (_ : Higlo.lexer) = Higlo.get_lexer lang in
            Some (
              "<pre>"
              ^ (Higlo.to_xtmpl ~lang code |> Xtmpl.string_of_xmls)
              ^ "</pre>")
          with e ->
            eprintf "Exn: %s\n%!" (Printexc.to_string e);
            default ()
          end
        | _ -> default ()
        end
      | Paragraph [Img (alt, src, title)] ->
        eprintf "image: %s, %s, %s\n%!" alt src title;
        global_images := src :: !global_images;
        let img =
          sprintf "<img class=\"stretch\" src=%S alt=%S />" src alt
        in
        Some img
      | omd_elt -> None
    in
    Omd.to_html ~override md
  in
  Omd.of_string s |> omd_to_html

let render markdown =
  sprintf "<div class=\"reveal\">\
          <div class=\"slides\">\n%s</section></div></div>"
    (markdown_to_html markdown)

let template =
  [%blob "src/template.html"]
let reveal =
  [%blob "src/css/reveal.css"]
let reveal_theme =
  [%blob "src/css/theme/serif.css"]
    
let sinai_main = [%blob "src/img/bg-sinai-main.png"]
let sinai_front = [%blob "src/img/bg-sinai-front.png"]

let js =
  [%blob "src/js/head.min.js"]
  ^
  [%blob "src/js/reveal.min.js"]

let cmdf fmt =
  ksprintf (fun s ->
      eprintf "CMD: %S\n%!" s;
      match Sys.command s with
      | 0 -> ()
      | other ->
        ksprintf failwith "CMD failed: %S returned %d" s other) fmt

let () =
  let source = Buffer.create 42 in
  let input_file = Sys.argv.(1) in
  let i = open_in input_file in
  let rec sloop () =
    try
      (Buffer.add_char source (input_char i);
       sloop ()) 
    with _ -> () in
  sloop ();
  close_in i;
  let html = Buffer.contents source |> render in
  let output = Buffer.create 42 in
  let apply_variables =
    function
    | "template_head_title" -> !global_title
    | "template_body" -> html
    | other ->
      eprintf "Warning: unkown variable: %S\n%!" other;
      other
  in
  Buffer.add_substitute output apply_variables template;
  cmdf "mkdir -p %S" Sys.argv.(2);
  let outfile f = Filename.concat Sys.argv.(2) f in
  let o = open_out (outfile "index.html") in
  Buffer.output_buffer o output;
  close_out o;
  let css = open_out (outfile "style.css") in
  output_string css reveal;
  output_string css "\n";
  output_string css reveal_theme;
  close_out css;
  let img_main = open_out (outfile "bg-main.png") in
  output_string img_main sinai_main;
  close_out img_main;
  let img_front = open_out (outfile "bg-front.png") in
  output_string img_front sinai_front;
  close_out img_front;
  let code = open_out (outfile "code.js") in
  output_string code js;
  close_out code;
  List.iter !global_images ~f:(fun src ->
      if Filename.is_relative src
      then begin
        let from = Filename.dirname input_file in
        cmdf "mkdir -p %s"
          Filename.(concat Sys.argv.(2) src |> dirname |> quote);
        cmdf "cp -f %s %s"
          Filename.(concat from src |> quote)
          Filename.(concat Sys.argv.(2) src |> quote)
      end
    );
  ()

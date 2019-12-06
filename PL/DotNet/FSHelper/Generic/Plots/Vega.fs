namespace CGFSHelper.Plots

module Vega =
    open System
    open System.IO
    open System.Runtime.InteropServices
    open System.Diagnostics

    let private content_string = "###CONTENT###"

    let private skeleton = """
<head>
<script src="https://cdn.jsdelivr.net/npm/vega@5"></script>
</head>
<body>
<div id="view"></div>

<script id="visualization" type="application/json">
###CONTENT###
</script>

<script type="text/javascript">
  var view;

  description = document.getElementById("visualization").innerHTML;
  spec = JSON.parse(description);
  render(spec);

  function render(spec) {
    view = new vega.View(vega.parse(spec), {
      renderer:  'canvas',  // renderer (canvas or svg)
      container: '#view',   // parent DOM container
      hover:     true       // enable hover processing
    });

    return view.runAsync();
  }

</script>
</body>
    """

    let private display_html_page (filename : string) =
        if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
            Process.Start(@"cmd.exe ", @"/c " + filename) |> ignore
        else
            // TODO: Need to implement process start for other OSes
            raise (NotImplementedException(sprintf "Do not know how to open %s in this platform" filename))

    let create_from_raw_specification (content : string) = skeleton.Replace(content_string, content)

    let plot_raw_specification (specification : string) =
        let temp_filename = Path.GetTempFileName().Replace(".tmp", ".html")
        let content = create_from_raw_specification specification
        File.WriteAllText(temp_filename, content)
        display_html_page temp_filename


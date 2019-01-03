use File::Which;

$pdf_mode = 1;

@default_files = ('{{cookiecutter.file_name}}.tex');

sub run_chktex {
  my $name = shift;
  if ( $silent ) {
    system "chktex -q -v0 $name";
  }
  else {
    system "chktex $name";
  };
}

sub run_lacheck {
  my $name = shift;
  system "lacheck $name";
}

sub lint {
  my $fname = shift;
  # lint
  run_chktex $fname;
  run_lacheck $fname;
  return system(@_);
}

$latex_silent_switch = "-interaction=batchmode -c-style-errors";
$pdflatex_silent_switch = "-interaction=batchmode -c-style-errors";
$biber_silent_switch = "--onlylog";
$bibtex_silent_switch = "-terse";
$dvips_silent_switch = "-q";

my $sumatra = which('sumatrapdf');
if (length $sumatra) {
  $pdf_previewer = "start sumatrapdf";
}

$pdflatex = "internal lint %S $pdflatex";
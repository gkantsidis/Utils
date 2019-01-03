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

$pdflatex = "internal lint %S $pdflatex";
use strict;
use warnings;

my ($infile, $outfile) = @ARGV;
open my $ih, '<', $infile or die "Could not open $infile for reading: $!";
open my $oh, '>', $outfile or die "Coult not open $outfile for writing: $!";

my ($nnodes, $nedges) = split / /, <$ih>;
print $oh "graph example {\n";
for(1..$nedges){
    chomp(my ($n1, $n2, $el) = split / /, <$ih>);
    print $oh "$n1 -- $n2 [label=\"$el\"]\n";
}
print $oh "}\n";

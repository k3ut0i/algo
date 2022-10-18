use strict;
use Data::Dumper;
sub find_string{
    my ($str, $pt) = @_;
    my @chars = split //, $str;
    for(@chars){
        if(defined $pt) { $pt = $pt->{$_}; }
        else { return undef;}
    }
    return 1;
};
sub add_string {
    my ($str, $pt) = @_;
    my @chars = split //, $str;
    for(@chars){
        if(defined $pt) {
            if (!defined $pt->{$_}) {$pt->{$_} = {};}
            $pt = $pt->{$_};
        }
        else {$pt = {$_ => {}}; $pt = $pt->{$_};}
    }
    $pt->{count} = defined $pt->{count} ? $pt->{count} + 1 : 1;
}
my $pt = {};
my @words = ("raja", "rani", "rama", "raghu");
for(@words) { add_string($_, $pt); }
print Dumper($pt);

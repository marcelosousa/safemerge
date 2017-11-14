#!/usr/bin/perl

use strict;
use warnings;

open(my $info, '-|', 'find', '.', '-type', 'f', '-name', '*_o.java', '-print') or die "Failed: $!";

while(my $line = <$info>) {
  my @words = split ' ', $line;
  my $bench = substr($words[0],0,-7);
  my @dirs = split '/', $bench;
  my $dir = "$dirs[0]/$dirs[1]/$dirs[2]/";
  my $file = $dirs[3];
  print "Processing $dir\n";
 # print "Processing file $file\n";
  system("cd $dir; time wiz verify -b $file -m=Dep > wiz.log");
}

close $info;


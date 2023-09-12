#!/usr/bin/perl
use strict;
use warnings;
use Scalar::Util qw(looks_like_number);
use List::Util qw(min max sum);
use Time::Piece;

print "What is the filename of the data from Hilltop Hydro? ";
my $filename = <STDIN>; 
chomp($filename);

my $today = Time::Piece -> new() -> strftime('%Y%m%d');
my $hydrotel_update = "Site Name,Minimum,Maximum,Average\n";

open my $file, '<', "$filename" or die ("cant open file: $!");
my $text;
while(defined(my $line = <$file>)) {
      chomp($line);  #cleans off any whitespaces and newlines
      $text .= "$line\n";
}
close $file;

my @site_split = split /Source is /, $text;
shift @site_split; #gets rid of the top header info about Hilltop Hydro

foreach my $site (@site_split) {
      my @header_split = split /Year[ \t]+Month[ \t]+Minimum[ \t]+Mean[ \t]+Maximum[ \t]+Std Dev[ \t]+20th \%[ \t]+25th \%[ \t]+Median[ \t]+75th \%[ \t]+80th \%\n/, $site; #split header and data

      #get sitename from header
      my $name_start = index $header_split[0], "(mm) at";
      my $name_end = index $header_split[0], "Monthly";
      my $sitename = substr $header_split[0], $name_start + 8, $name_end-$name_start-9;

      #splits each data line
      my @dataline_split = split /\n/, $header_split[1];
      #invoke all the min/max/mean arrays
      my (@JanMin, @JanMax, @JanMean, @FebMin, @FebMax, @FebMean, @MarMin, @MarMax, @MarMean, @AprMin, @AprMax, @AprMean,
          @MayMin, @MayMax, @MayMean, @JunMin, @JunMax, @JunMean, @JulMin, @JulMax, @JulMean, @AugMin, @AugMax, @AugMean,
          @SepMin, @SepMax, @SepMean, @OctMin, @OctMax, @OctMean, @NovMin, @NovMax, @NovMean, @DecMin, @DecMax, @DecMean,
          @AllMin, @AllMax, @AllMean);
      #populate the min/max/mean arrays
      foreach my $dataline (@dataline_split) {
            my @data_split = split /[ \t]+/, $dataline;
            my $year = $data_split[0]; my $month = $data_split[1]; 
            my $min = $data_split[2]; my $mean = $data_split[3]; my $max = $data_split[4];
            if (looks_like_number($min) && looks_like_number($max) && looks_like_number($mean)) { #check that they're numbers and not errors
                  if ($month == 1) {push @JanMin, $min; push @JanMax, $max; push @JanMean, $mean;}
                  if ($month == 2) {push @FebMin, $min; push @FebMax, $max; push @FebMean, $mean;}
                  if ($month == 3) {push @MarMin, $min; push @MarMax, $max; push @MarMean, $mean;}
                  if ($month == 4) {push @AprMin, $min; push @AprMax, $max; push @AprMean, $mean;}
                  if ($month == 5) {push @MayMin, $min; push @MayMax, $max; push @MayMean, $mean;}
                  if ($month == 6) {push @JunMin, $min; push @JunMax, $max; push @JunMean, $mean;}
                  if ($month == 7) {push @JulMin, $min; push @JulMax, $max; push @JulMean, $mean;}
                  if ($month == 8) {push @AugMin, $min; push @AugMax, $max; push @AugMean, $mean;}
                  if ($month == 9) {push @SepMin, $min; push @SepMax, $max; push @SepMean, $mean;}
                  if ($month == 10) {push @OctMin, $min; push @OctMax, $max; push @OctMean, $mean;}
                  if ($month == 11) {push @NovMin, $min; push @NovMax, $max; push @NovMean, $mean;}
                  if ($month == 12) {push @DecMin, $min; push @DecMax, $max; push @DecMean, $mean;}
                  push @AllMin, $min; push @AllMax, $max; push @AllMean, $mean;
            }
      }
      #get monthly and total min/max/means
      my ($JanMinMin, $JanMaxMax, $JanMeanMean, $FebMinMin, $FebMaxMax, $FebMeanMean, $MarMinMin, $MarMaxMax, $MarMeanMean,
          $AprMinMin, $AprMaxMax, $AprMeanMean, $MayMinMin, $MayMaxMax, $MayMeanMean, $JunMinMin, $JunMaxMax, $JunMeanMean,
          $JulMinMin, $JulMaxMax, $JulMeanMean, $AugMinMin, $AugMaxMax, $AugMeanMean, $SepMinMin, $SepMaxMax, $SepMeanMean,
          $OctMinMin, $OctMaxMax, $OctMeanMean, $NovMinMin, $NovMaxMax, $NovMeanMean, $DecMinMin, $DecMaxMax, $DecMeanMean);
      if (scalar @JanMean > 0) { $JanMinMin = min @JanMin; $JanMaxMax = max @JanMax; $JanMeanMean = (sum @JanMean)/(scalar @JanMean); }
      if (scalar @FebMean > 0) { $FebMinMin = min @FebMin; $FebMaxMax = max @FebMax; $FebMeanMean = (sum @FebMean)/(scalar @FebMean); }
      if (scalar @MarMean > 0) { $MarMinMin = min @MarMin; $MarMaxMax = max @MarMax; $MarMeanMean = (sum @MarMean)/(scalar @MarMean); }
      if (scalar @AprMean > 0) { $AprMinMin = min @AprMin; $AprMaxMax = max @AprMax; $AprMeanMean = (sum @AprMean)/(scalar @AprMean); }
      if (scalar @MayMean > 0) { $MayMinMin = min @MayMin; $MayMaxMax = max @MayMax; $MayMeanMean = (sum @MayMean)/(scalar @MayMean); }
      if (scalar @JunMean > 0) { $JunMinMin = min @JunMin; $JunMaxMax = max @JunMax; $JunMeanMean = (sum @JunMean)/(scalar @JunMean); }
      if (scalar @JulMean > 0) { $JulMinMin = min @JulMin; $JulMaxMax = max @JulMax; $JulMeanMean = (sum @JulMean)/(scalar @JulMean); }
      if (scalar @AugMean > 0) { $AugMinMin = min @AugMin; $AugMaxMax = max @AugMax; $AugMeanMean = (sum @AugMean)/(scalar @AugMean); }
      if (scalar @SepMean > 0) { $SepMinMin = min @SepMin; $SepMaxMax = max @SepMax; $SepMeanMean = (sum @SepMean)/(scalar @SepMean); }
      if (scalar @OctMean > 0) { $OctMinMin = min @OctMin; $OctMaxMax = max @OctMax; $OctMeanMean = (sum @OctMean)/(scalar @OctMean); }
      if (scalar @NovMean > 0) { $NovMinMin = min @NovMin; $NovMaxMax = max @NovMax; $NovMeanMean = (sum @NovMean)/(scalar @NovMean); }
      if (scalar @DecMean > 0) { $DecMinMin = min @DecMin; $DecMaxMax = max @DecMax; $DecMeanMean = (sum @DecMean)/(scalar @DecMean); }
      my $AllMinMin = min @AllMin; my $AllMaxMax = max @AllMax; my $AllMeanMean = (sum @AllMean)/(scalar @AllMean);

      #get years of each monthly min/max
      my ($YearMinJan, $YearMaxJan, $YearMinFeb, $YearMaxFeb, $YearMinMar, $YearMaxMar, $YearMinApr, $YearMaxApr,
          $YearMinMay, $YearMaxMay, $YearMinJun, $YearMaxJun, $YearMinJul, $YearMaxJul, $YearMinAug, $YearMaxAug, 
          $YearMinSep, $YearMaxSep, $YearMinOct, $YearMaxOct, $YearMinNov, $YearMaxNov, $YearMinDec, $YearMaxDec);
      foreach my $dataline (@dataline_split) {
            my @data_split = split /[ \t]+/, $dataline;
            my $year = $data_split[0]; my $month = $data_split[1]; 
            my $min = $data_split[2]; my $mean = $data_split[3]; my $max = $data_split[4];
            if (looks_like_number($min) && looks_like_number($max) && looks_like_number($mean)) { #check that they're numbers and not errors
                  if ($month == 1 && $min == $JanMinMin && !defined $YearMinJan) { $YearMinJan = $year; }
                  if ($month == 1 && $max == $JanMaxMax && !defined $YearMaxJan) { $YearMaxJan = $year; }
                  if ($month == 2 && $min == $FebMinMin && !defined $YearMinFeb) { $YearMinFeb = $year; }
                  if ($month == 2 && $max == $FebMaxMax && !defined $YearMaxFeb) { $YearMaxFeb = $year; }
                  if ($month == 3 && $min == $MarMinMin && !defined $YearMinMar) { $YearMinMar = $year; }
                  if ($month == 3 && $max == $MarMaxMax && !defined $YearMaxMar) { $YearMaxMar = $year; }
                  if ($month == 4 && $min == $AprMinMin && !defined $YearMinApr) { $YearMinApr = $year; }
                  if ($month == 4 && $max == $AprMaxMax && !defined $YearMaxApr) { $YearMaxApr = $year; }
                  if ($month == 5 && $min == $MayMinMin && !defined $YearMinMay) { $YearMinMay = $year; }
                  if ($month == 5 && $max == $MayMaxMax && !defined $YearMaxMay) { $YearMaxMay = $year; }
                  if ($month == 6 && $min == $JunMinMin && !defined $YearMinJun) { $YearMinJun = $year; }
                  if ($month == 6 && $max == $JunMaxMax && !defined $YearMaxJun) { $YearMaxJun = $year; }
                  if ($month == 7 && $min == $JulMinMin && !defined $YearMinJul) { $YearMinJul = $year; }
                  if ($month == 7 && $max == $JulMaxMax && !defined $YearMaxJul) { $YearMaxJul = $year; }
                  if ($month == 8 && $min == $AugMinMin && !defined $YearMinAug) { $YearMinAug = $year; }
                  if ($month == 8 && $max == $AugMaxMax && !defined $YearMaxAug) { $YearMaxAug = $year; }
                  if ($month == 9 && $min == $SepMinMin && !defined $YearMinSep) { $YearMinSep = $year; }
                  if ($month == 9 && $max == $SepMaxMax && !defined $YearMaxSep) { $YearMaxSep = $year; }
                  if ($month == 10 && $min == $OctMinMin && !defined $YearMinOct) { $YearMinOct = $year; }
                  if ($month == 10 && $max == $OctMaxMax && !defined $YearMaxOct) { $YearMaxOct = $year; }
                  if ($month == 11 && $min == $NovMinMin && !defined $YearMinNov) { $YearMinNov = $year; }
                  if ($month == 11 && $max == $NovMaxMax && !defined $YearMaxNov) { $YearMaxNov = $year; }
                  if ($month == 12 && $min == $DecMinMin && !defined $YearMinDec) { $YearMinDec = $year; }
                  if ($month == 12 && $max == $DecMaxMax && !defined $YearMaxDec) { $YearMaxDec = $year; }
            }
      }

    my @findstart = split /[ \t]+/, $dataline_split[0];
    my $startyear = $findstart[0];
    my $startmonth = $findstart[1];
    my $lastline = scalar @dataline_split;
    my @findend = split /[ \t]+/, $dataline_split[$lastline-1];
    my $endyear = $findend[0];
    my $endmonth = $findend[1];

      # make html table
      my $table = "<P><h2>Site: $sitename</h2></p>
      <P><strong>Period of analysis:</strong> $startmonth $startyear to $endmonth $endyear</P>
<table width=\"831\"><tbody>
<tr><td width=\"63\">&nbsp;</td>
<td style=\"text-align: center;\" colspan=\"12\" width=\"768\"><strong>Month</strong></td></tr>
<tr><td width=\"63\">&nbsp;</td>
<td width=\"64\"><strong>Jan</strong></td>
<td width=\"64\"><strong>Feb</strong></td>
<td width=\"64\"><strong>Mar</strong></td>
<td width=\"64\"><strong>Apr</strong></td>
<td width=\"64\"><strong>May</strong></td>
<td width=\"64\"><strong>Jun</strong></td>
<td width=\"64\"><strong>Jul</strong></td>
<td width=\"64\"><strong>Aug</strong></td>
<td width=\"64\"><strong>Sep</strong></td>
<td width=\"64\"><strong>Oct</strong></td>
<td width=\"64\"><strong>Nov</strong></td>
<td width=\"64\"><strong>Dec</strong></td></tr>
<tr><td width=\"63\">Min level (m)</td>
<td width=\"64\">" . sprintf("%.2f",$JanMinMin*.001) . "</td>
<td width=\"64\">" . sprintf("%.2f",$FebMinMin*.001) . "</td>
<td width=\"64\">" . sprintf("%.2f",$MarMinMin*.001) . "</td>
<td width=\"64\">" . sprintf("%.2f",$AprMinMin*.001) . "</td>
<td width=\"64\">" . sprintf("%.2f",$MayMinMin*.001) . "</td>
<td width=\"64\">" . sprintf("%.2f",$JunMinMin*.001) . "</td>
<td width=\"64\">" . sprintf("%.2f",$JulMinMin*.001) . "</td>
<td width=\"64\">" . sprintf("%.2f",$AugMinMin*.001) . "</td>
<td width=\"64\">" . sprintf("%.2f",$SepMinMin*.001) . "</td>
<td width=\"64\">" . sprintf("%.2f",$OctMinMin*.001) . "</td>
<td width=\"64\">" . sprintf("%.2f",$NovMinMin*.001) . "</td>
<td width=\"64\">" . sprintf("%.2f",$DecMinMin*.001) . "</td></tr>
<tr><td width=\"63\">Year it occurred</td>
<td width=\"64\">$YearMinJan</td>
<td width=\"64\">$YearMinFeb</td>
<td width=\"64\">$YearMinMar</td>
<td width=\"64\">$YearMinApr</td>
<td width=\"64\">$YearMinMay</td>
<td width=\"64\">$YearMinJun</td>
<td width=\"64\">$YearMinJul</td>
<td width=\"64\">$YearMinAug</td>
<td width=\"64\">$YearMinSep</td>
<td width=\"64\">$YearMinOct</td>
<td width=\"64\">$YearMinNov</td>
<td width=\"64\">$YearMinDec</td></tr>
<tr><td width=\"63\">Max level (m)</td>
<td width=\"64\">" . sprintf("%.2f",$JanMaxMax*.001) . "</td>
<td width=\"64\">" . sprintf("%.2f",$FebMaxMax*.001) . "</td>
<td width=\"64\">" . sprintf("%.2f",$MarMaxMax*.001) . "</td>
<td width=\"64\">" . sprintf("%.2f",$AprMaxMax*.001) . "</td>
<td width=\"64\">" . sprintf("%.2f",$MayMaxMax*.001) . "</td>
<td width=\"64\">" . sprintf("%.2f",$JunMaxMax*.001) . "</td>
<td width=\"64\">" . sprintf("%.2f",$JulMaxMax*.001) . "</td>
<td width=\"64\">" . sprintf("%.2f",$AugMaxMax*.001) . "</td>
<td width=\"64\">" . sprintf("%.2f",$SepMaxMax*.001) . "</td>
<td width=\"64\">" . sprintf("%.2f",$OctMaxMax*.001) . "</td>
<td width=\"64\">" . sprintf("%.2f",$NovMaxMax*.001) . "</td>
<td width=\"64\">" . sprintf("%.2f",$DecMaxMax*.001) . "</td></tr>
<tr><td width=\"63\">Year it occurred</td>
<td width=\"64\">$YearMaxJan</td>
<td width=\"64\">$YearMaxFeb</td>
<td width=\"64\">$YearMaxMar</td>
<td width=\"64\">$YearMaxApr</td>
<td width=\"64\">$YearMaxMay</td>
<td width=\"64\">$YearMaxJun</td>
<td width=\"64\">$YearMaxJul</td>
<td width=\"64\">$YearMaxAug</td>
<td width=\"64\">$YearMaxSep</td>
<td width=\"64\">$YearMaxOct</td>
<td width=\"64\">$YearMaxNov</td>
<td width=\"64\">$YearMaxDec</td></tr>
<tr><td width=\"63\">Average</td>
<td width=\"64\">" . sprintf("%.2f",$JanMeanMean*.001) . "</td>
<td width=\"64\">" . sprintf("%.2f",$FebMeanMean*.001) . "</td>
<td width=\"64\">" . sprintf("%.2f",$MarMeanMean*.001) . "</td>
<td width=\"64\">" . sprintf("%.2f",$AprMeanMean*.001) . "</td>
<td width=\"64\">" . sprintf("%.2f",$MayMeanMean*.001) . "</td>
<td width=\"64\">" . sprintf("%.2f",$JunMeanMean*.001) . "</td>
<td width=\"64\">" . sprintf("%.2f",$JulMeanMean*.001) . "</td>
<td width=\"64\">" . sprintf("%.2f",$AugMeanMean*.001) . "</td>
<td width=\"64\">" . sprintf("%.2f",$SepMeanMean*.001) . "</td>
<td width=\"64\">" . sprintf("%.2f",$OctMeanMean*.001) . "</td>
<td width=\"64\">" . sprintf("%.2f",$NovMeanMean*.001) . "</td>
<td width=\"64\">" . sprintf("%.2f",$DecMeanMean*.001) . "</td></tr>
</tbody></table>
<P><strong>Groundwater summary for Hydrotel</strong></p>
Minimum: " . sprintf("%.3f",$AllMinMin*.001) . "<br><br>
Average: " . sprintf("%.3f",$AllMeanMean*.001) . "<br>
Maximum: " . sprintf("%.3f",$AllMaxMax*.001) . "<br>";

# output table to html file
open my $sitedata, '>', "$today\_GroundwaterLevel\_$sitename.html" or die ("cant open data file: $!");
print $sitedata "$table";
close $sitedata;

# hydrotel output
$hydrotel_update .= $sitename .",". sprintf("%.3f",$AllMinMin*.001) .",". sprintf("%.3f",$AllMaxMax*.001) .",". sprintf("%.3f",$AllMeanMean*.001) ."\n";

} #closes site loop

# output hydrotel to file
open my $hydrotel_file, '>', "$today\_GroundwaterLevel\_Hydrotel.csv" or die ("cant open data file: $!");
print $hydrotel_file "$hydrotel_update";
close $hydrotel_file;

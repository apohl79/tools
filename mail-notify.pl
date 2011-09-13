#!/usr/bin/perl

use strict;
use Number::Format qw(:subs);
use Mail::Message::Field::Full;

my $notify_long = "/usr/bin/notify-send -u critical -i kmail -t 30000 ";
my $notify_short = "/usr/bin/notify-send -u normal -i kmail -t 10000 ";
my $notify_middle = "/usr/bin/notify-send -u normal -i kmail -t 20000 ";
my $notify_appointment = "/usr/bin/notify-send -u low -i korganizer -t 900000 ";
my $subject;
my $body;
my $from;
my $date;
my $folder;
my $info;
my $show_long = 0;
my $show_appointment = 0;

if( $ARGV[0] eq "--impressions" ) {
  doImpressions();
} elsif( $ARGV[0] eq "--reminder" ) {
  doReminder();
} else {
  doBackground();
}

sub doBackground {
  # Normal mode, parse procmail log
  open LOG, "<$ENV{HOME}/.procmail.log";
  seek LOG, 0, 2;

  while ( 1 ) {
    $_ = <LOG>;
    chomp;
    if ( length $_ ) {
      if ( /Subject\: (.*)$/i ) {
	$body = Mail::Message::Field::Full->decode( $1 );
	#print "body=$body\n";
	if ( $body =~ /(adserv|adssl|ad\d\d\d|select|deliverydb|repsrv|cache|opensw).*(CRITICAL|crashed)/ ) {
	  $subject = uc( $1 );
	  $subject =~ s/"//g;
	  $subject =~ s/>/&gt;/g;
	  $subject =~ s/</&lt;/g;
	  #print "subject=$subject\n";
	  $show_long = 1;
	}
	$body =~ s/<br>/\n/g;
	$body =~ s/<tab>/\t/g;
      } elsif ( /From ([^ ]+) (.*)$/i ) {
	$from = $1;
	$date = $2;
	$from =~ s/"//g;
	$from =~ s/>/&gt;/g;
	$from =~ s/</&lt;/g;
	#print "from=$from\n";
      } elsif ( /Folder\: \/home\/osar\/([^ ]+)\/new.*/ ) {
	$folder = $1;
	#print "folder='$folder'\n";
	if ( $folder eq "Maildir" or
	     $folder eq "Maildir/.Datacenter" or
	     $folder eq "Maildir/.Services" ) {
	  $info = "<i>$from</i>\n<u>$folder</u>\n<b>$body</b>";
	  if ( $show_long ) {
	    #print "$notify_long \"$subject\" \"$info\"\n";
	    system "$notify_long \"$subject - $date\" \"$info\"";
	  } else {
	    #print "$notify_short \"Info\" \"$info\"\n";
	    system "$notify_short \"Info - $date\" \"$info\"";
	  }
	}
	$show_long = 0;
      }
    } else {
      sleep 5;
      seek F, 0, 1;
    }
    while( my $f=</tmp/mail-notify-*> ) {
      chomp $f;
      if( $f =~ /mail-notify-(\d+)$/ ) {
	my $ts = $1;
	if( time() - $ts < 60 ) {
	  open NOTIFY, "<$f";
	  my @cmd = <NOTIFY>;
	  close NOTIFY;
	  system join( "", @cmd );
	}
	unlink $f;
      }
    }
  }

  close F;
}

sub doReminder {
  my @mail = <STDIN>;
  my $type = "";
  my $title = "";
  my $start = "";
  my $end = "";
  my $dur = "";
  my $loc = "";
  foreach my $line (@mail) {
    if( $line =~ /^Start: (.*)$/ ) {
      $start = $1;
    } elsif( $line =~ /^End: (.*)$/ ) {
      $end = $1;
    } elsif( $line =~ /^Duration: (.*)$/ ) {
      $dur = $1;
    } elsif( $line =~ /^Location: (.*)$/ ) {
      $loc = $1;
    } elsif( $line =~ /^Type: (.*)$/ ) {
      $type = $1;
    } elsif( $line =~ /^Title: (.*)$/ ) {
      $title = $1;
    }
  }
  open NOTIFY, ">/tmp/mail-notify-".time();
  print NOTIFY "$notify_appointment \"$type - $start\" \"<b>Title:</b>\t\t$title\n<b>Duration:</b>\t$dur\n<b>Location:</b>\t$loc\"";
  close NOTIFY;
  print join( "", @mail );
}

sub doImpressions {
  my @mail = <STDIN>;
  my $lastweek = 0;
  my $yesterday = 0;
  for( my $i = 0; $i <= $#mail; $i++ ) {
    if( index( $mail[$i], "COMPARE TO LASTWEEK" ) > -1 ) {
      $lastweek = 1;
      $i++;
    } elsif( index( $mail[$i], "COMPARE TO YESTERDAY" ) > -1 ) {
      $yesterday = 1;
      $i++;
    } elsif( $lastweek == 1 && index( $mail[$i], "-----" ) == 0 ) {
      $lastweek = $mail[$i-1];
      chomp $lastweek;
      $lastweek =~ s/^ +//g;
      $lastweek =~ s/ +/ /g;
      my( $day, $lw, $today, $perc ) = split / /, $lastweek;
      $lw = format_picture( $lw, '###,###,###,###' );
      $lw =~ s/^ +//;
      $today = format_picture( $today, '###,###,###,###' );
      $today =~ s/^ +//;
      $lastweek = " \t$lw\t<b>$today\t$perc</b>\n";
      my $sum = $mail[$i+1];
      chomp $sum;
      $sum =~ s/ +/ /g;
      ( $day, $lw, $today, $perc ) = split / /, $sum;
      $lw = format_picture( $lw, '###,###,###,###' );
      $lw =~ s/^ +//;
      $today = format_picture( $today, '###,###,###,###' );
      $today =~ s/^ +//;
      $lastweek.= "<b>S:</b>\t$lw\t<b>$today\t$perc</b>";
    } elsif( $yesterday == 1 && index( $mail[$i], "-----" ) == 0 ) {
      $yesterday = $mail[$i-1];
      chomp $yesterday;
      $yesterday =~ s/^ +//g;
      $yesterday =~ s/ +/ /g;
      my( $day, $lw, $today, $perc ) = split / /, $yesterday;
      $lw = format_picture( $lw, '###,###,###,###' );
      $lw =~ s/^ +//;
      $today = format_picture( $today, '###,###,###,###' );
      $today =~ s/^ +//;
      $yesterday = " \t$lw\t<b>$today\t$perc</b>\n";
      my $sum = $mail[$i+1];
      chomp $sum;
      $sum =~ s/ +/ /g;
      ( $day, $lw, $today, $perc ) = split / /, $sum;
      $lw = format_picture( $lw, '###,###,###,###' );
      $lw =~ s/^ +//;
      $today = format_picture( $today, '###,###,###,###' );
      $today =~ s/^ +//;
      $yesterday.= "<b>S:</b>\t$lw\t<b>$today\t$perc</b>";
    }
  }
  open NOTIFY, ">/tmp/mail-notify-".time();
  print NOTIFY "$notify_middle \"Impressions $ARGV[1]\" \"<b>Last Week:</b>\n$lastweek\n\n<b>Yesterday:</b>\n$yesterday\"";
  close NOTIFY;
  print join( "", @mail );
}

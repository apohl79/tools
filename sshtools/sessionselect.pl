#!/usr/bin/perl
#
# (c) 2008 Andreas Pohl

use Gtk2;
use Gtk2::SimpleList;
use strict;

$|++;

my $sessiondir = $ARGV[0];
my $state_connected = "YES";
my $state_disconnected = "";
my %machines_connected;

$sessiondir ||= "$ENV{HOME}/.sshsessions";
if (!-d $sessiondir) {
  mkdir $sessiondir;
}

print "Initializing GUI...\n";

# loading old position
my ($lastX, $lastY) = (-1, -1);
if (open F, "<$sessiondir/.sessionselect") {
  my $geo = <F>;
  ($lastX, $lastY) = split /,/, $geo;
  close F;
}

set_locale Gtk2;  # internationalize
init Gtk2;        # initialize Gtk-Perl

# widget creation
my $window = new Gtk2::Window("toplevel");
my $mainbox = Gtk2::VBox->new();
my $entry = new Gtk2::Entry();
my $slist = Gtk2::SimpleList->new("Machines"." "x65 => 'text',
				  "Connected" => 'text');

# callback registration
$window->signal_connect("delete_event", \&closeAppWindow);
$entry->signal_connect("changed", \&handleChange);
$entry->signal_connect("key-press-event", \&handleSelectKey);
$slist->signal_connect("button-release-event", \&handleSelectClick);
$slist->signal_connect("key-press-event", \&handleSelectKey);

# show entry
$entry->set_width_chars(50);
$entry->show();

# show listbox
$slist->show();

# show connected machines on startup
print "Getting open sessions...\n";
getConnected();

# boxes
$mainbox->pack_start($entry, 0, 0, 0);
$mainbox->pack_start($slist, 0, 0, 0);
$mainbox->show();

# set window attributes and show it
$window->set_title("Select SSH Session");
$window->set_border_width(15);
$window->add($mainbox);
if ($lastX > -1 && $lastY > -1) {
  $window->move($lastX, $lastY);
}
$window->show();

# Gtk event loop
print "Staring main loop...\n";
main Gtk2;

# Should never get here
exit( 0 );


sub connectMachine {
  my @sel = $slist->get_selected_indices;
  if (scalar @sel) {
    my $machine = $slist->{data}[$sel[0]][0];
    print "connecting $machine\n";
    system "$ENV{HOME}/bin/sshtools/opensession.sh $ENV{HOME}/.sshsessions/$machine";
  }
  closeAppWindow();
}

sub getConnected {
  my %pids;
  foreach (split /\n/, `ps -e|awk '{print \$1}'`) {
    $pids{$_} = 1;
  }
  my $count = 1; # _local_ will alway be there
  push @{$slist->{data}}, ["_local_", $state_connected];
  %machines_connected = ("_local_" => 0);
  while (my $tmpfile = </tmp/sshtools.*>) {
    chomp $tmpfile;
    my $machine = $tmpfile;
    $machine =~ s|/tmp/sshtools\.||;
    $machine =~ s|(.*)\.(\d+)$|$1|;
    my $pid = $2;
    if (exists $pids{$pid} && !exists $machines_connected{$machine}) {
      push @{$slist->{data}}, [$machine, $state_connected];
    }
    if (!exists $pids{$pid} && $machine ne "_local_") {
      unlink $tmpfile;
    }
    push @{$machines_connected{$machine}}, $pid;
    $count++;
  }
  if ($count) {
    $slist->select(0);
  }
}

### Callback function to close the window
sub closeAppWindow
{
  my ($x, $y) = $window->get_position();
  open F, ">$sessiondir/.sessionselect";
  print F "$x,$y";
  close F;
  Gtk2->main_quit;
  return 0;
}

sub handleChange {
  my $entry = shift;
  my $text = $entry->get_text;
  my @machines;
  if (length($text) > 2) {
    opendir D, $sessiondir;
    foreach my $d (sort readdir(D)) {
      if (index($d, $text) == 0) {
	push @machines, $d;
      }
    }
  }
  my @empty;
  @{$slist->{data}} = @empty;
  if (scalar @machines) {
    foreach my $m (@machines) {
      my $state = exists $machines_connected{$m}? $state_connected: $state_disconnected;
      push @{$slist->{data}}, [$m, $state];
    }
    $slist->select(0);
  } else {
    getConnected();
  }
  my ($w, $h) = $window->get_size();
  $window->resize($w, 10);
}

sub handleSelectClick {
  my ($slist, $event) = @_;
  if ($event->button == 1) { # left click
    connectMachine();
  }
}

sub handleSelectKey {
  my ($slist, $event) = @_;
  if ($event->hardware_keycode == 36 || $event->hardware_keycode == 108) { # enter
    connectMachine();
  } elsif ($event->hardware_keycode == 9) { # esc
    closeAppWindow();
  }
}

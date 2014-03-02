#!perl -T
use strict;
use warnings FATAL => 'all';
use Test::More tests => 2;

BEGIN {
    push(@INC, 'inc');
    use_ok( 'MarpaX::Mof2Text' ) || print "Bail out!\n";
}

my $input = do { local $/; <DATA> };
my $t = MarpaX::Mof2Text->new();
$t->parse(\$input);

my $oups = <<OUPS;
[module DDLgen(RDBMS)/]
[template public SchemaToDDL (s: Schema)]
[for (t:Table | s.table)]
[TableToDDL(t)/]
[/for]
[/template]

[template public TableToDDL(t: Table)]
CREATE TABLE [t.name/] (
[for (c:Column|t.column) separator(',')]
[c.name/] [c.type/]
[/for]
);
[KeyToDDL(t.key)/]
[foreignKeyToDDL(t.foreignKey)/]
[/template]

[template private KeyToDDL(k:Key)]
ALTER TABLE [k.owner.name/] ADD (
CONSTRAINT [k.name/] PRIMARY KEY ([for (c:Column|k.column) separator(',')]c.name[/for]) 
);
[/template]

[template private ForeignKeyToDDL(fk:ForeignKey)]
ALTER TABLE [fk.owner.name/] ADD (
CONSTRAINT [fk.name/] FOREIGN KEY ([for (c:Column|fk.column) separator(',')]c.name[/for]) 

REFERENCES [fk.refersTo.owner.name/] ([for (c:Column|fk.refersTo.column) separator(',')]c.name[/for]) 
ON DELETE CASCADE
);
[/template]
[/module]
OUPS

__DATA__

[module DDLgen(RDBMS)/]
[template public SchemaToDDL (s: Schema)]
[for (t:Table | s.table)]
[TableToDDL(t)/]
[/for]
[/template]

[template public TableToDDL(t: Table)]
CREATE TABLE [t.name/] (
[for (c:Column|t.column) separator(',')]
[c.name/] [c.type/]
[/for]
);
[KeyToDDL(t.key)/]
[foreignKeyToDDL(t.foreignKey)/]
[/template]

[template private KeyToDDL(k:Key)]
ALTER TABLE [k.owner.name/] ADD (
CONSTRAINT [k.name/] PRIMARY KEY ([for (c:Column|k.column) separator(',')]c.name[/for]) 
);
[/template]

[template private ForeignKeyToDDL(fk:ForeignKey)]
ALTER TABLE [fk.owner.name/] ADD (
CONSTRAINT [fk.name/] FOREIGN KEY ([for (c:Column|fk.column) separator(',')]c.name[/for]) 

REFERENCES [fk.refersTo.owner.name/] ([for (c:Column|fk.refersTo.column) separator(',')]c.name[/for]) 
ON DELETE CASCADE
);
[/template]
[/module]

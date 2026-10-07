-- | Informational messages.
module SugarCane.Info where

versionString :: String
versionString = "0.1.0"

shortHelpMessage' :: String
shortHelpMessage' =
  "Minecraft sugar cane farm designer.\n\
  \\n\
  \usage: sugarc [-h|--help] from <SHAPE> [OPTIONS]\n\
  \\n\
  \Options:\n\
  \  -b, -block MASK          remove a section of the farm (see section `Mask Arguments`).\n\
  \  -a, -allow MASK          add a section to the farm (see section `Mask Arguments`).\n\
  \  -g, -gravity ALIGNMENT   set the alignment for subsequent masks (see section `Gravity Alignment`.\n\
  \  -e, -emit TYPE           how to display selected farms.\n\
  \                             Accepted Values:\n\
  \                             • stdout/-\n\
  \  -t, -take STRATEGY       which farms to display.\n\
  \                             Accepted Values:\n\
  \                             • one: show the first optimal layout\n\
  \                             • any: show all optimal layouts\n\
  \                             • all: show all layouts\n\
  \                             • farm: just show the farm without optimising\n\
  \                             • <INT>: number of layout [0..4]\
  \"

shortHelpMessage :: String
shortHelpMessage = shortHelpMessage' ++ "\n\nTry `sugarc --help` for more info."

helpMessage :: String
helpMessage =
  shortHelpMessage'
    ++ "\n\n"
    ++ "Mask Arguments:\n\
       \  Masks can be defined in one of three ways:\n\
       \    • <SHAPE>: place the defined shape in accordance with the current alignment settings.\n\
       \    • <SHAPE> <INT> <INT>: place the shape with its top left corner at the provided coordinates.\n\
       \    • <INT> <INT>: affect only the block at the provided coordinates.\n\
       \\n\
       \Shapes:\n\
       \  The following shapes can be used as arguments\n\
       \    • square <width>\n\
       \    • rectangle <width> <height>\n\
       \    • circle <diameter>\n\
       \    • ellipse <width> <height>\n\
       \\n\
       \Gravity Alignment:\n\
       \  sugarc defaults to vertical and horizontal centre alignment. The following values can be used to alter it:\n\
       \    • Vertical: top/horizon/bottom\n\
       \    • Horizonta: left/centre/right (center is also accepted)\n\
       \    • Combined: <VERTICAL>+<HORIZONTAL> or <VERTICAL>|<HORIZONTAL>\n\
       \\n\
       \Example Usage:\n\
       \  Get every optimal layout for a 10 wide square farm:\n\
       \    sugarc from square 10\n\
       \\n\
       \  Same as above but shorter:\n\
       \    sugarc -f square 10\n\
       \\n\
       \  Get a single optimal layout for a 10 wide circle farm:\n\
       \    sugarc from circle 10 -take one\n\
       \\n\
       \  Get a single optimal layout for an 11 wide square with a 3 wide square in the center removed:\n\
       \    sugarc from circle 11 -block square 3 -take one\n\
       \\n\
       \  Get all possible farms for a 10 by 15 rectangular farm with 2x2 squares removed from each corner:\n\
       \    sugarc from rectangle 10 15 \\\n\
       \        -gravity top+left \\\n\
       \        -block square 2 \\\n\
       \        -gravity top+right \\\n\
       \        -block square 2 \\\n\
       \        -gravity bottom+left \\\n\
       \        -block square 2 \\\n\
       \        -gravity bottom+right \\\n\
       \        -block square 2 \\\n\
       \        -take all\
       \"

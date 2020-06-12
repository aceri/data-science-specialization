# ----------------------------
# Regular Expressions:
# ----------------------------
# Metacharacters:
#     
#     ^   (start of a line)
# 
#            ^i think
# 
#                 i think we all rule for participating
#                 i think i have been outed
#                 i think this will be quite fun actually
#                 i think i need to go to work
#                 i think i first saw zombo in 1999
#
#     $   (end of a line)
#
#            morning$
#
#                 well they had something this morning
#                 then had to catch a tram home in the morning
#                 dog obedience school in the morning
#
# Character Classes with []
#
#     [Bb] [Uu] [Ss] [Hh]     (sets of characters we will accept at a given point)
#
#            The democracts are playing, "Name  the worst thing about Bush!"
#            I smelled the deser creosote bush, brownies, BBQ chicken
#            BBQ and bushwalking at Molonglo Gorge
#
#
# Combinationg of them
#
#
#'    ^[Ii] am
#'    
#'           i am so angry
#'           I am so angry
#'           I am whatever
#'           
#'    ^[0-9] [a-zA-Z]          
#'    
#'           7th inning stretch
#'           2nd half soon to begin
#'           3am - cant sleep
#'           5ft 7 sent from heaven
#'           
#'             
#' [^?.]$   ([^  indicates matching characters NOT in that class)
#'
#'           i like basketballs
#'           6 and 9
#'           dont worry... we all die anyway!
#'           Not in BAghdad
#'           
#' .     (the dot refers to any character)
#' 
#'      9.11
#'
#'           Its stupid the post 9-11 rules
#'           if any 1 of us did 9/11 we would have been caught in days
#'           NetBios: scanning ip 203.169.114.66
#'           Front Door 9:11:46 AM
#'           Sings: 0118999881999119725...3 !
#'           
#' |     (it translates to OR)            
#' 
#' 
#'      flood|fire
#'      
#'             is firewire like usb on none macs?
#'             the global flood makes sense within the context 
#'             yeah ive had the fire on tonight
#'             ... and the floods, hurricanes, killer heatwaves     
#'             
#'      flood|earthquake|hurricanes|coldfire
#'      
#'             Not a whole lot of hurricanes in the Arctic.
#'             We do have earthqueakes nearly eevery day somewhre
#'             hurricanes swirl in the other direction
#'             coldfire is straight !           
#'
#'
#' ^[Gg]ood|[Bb]ad
#'
#'      frases que empiecen por Good or 
#'         anywhere on the line Bad (primera letra en mays or mins)
#'
#'              good to hear
#'              Good afternoon
#'              Katie .... had bad experiences
#'              my middle name is trouble, Miss Bad News
#'              
#' [Gg]eorge([Ww]\.)? [Bb]ush
#' 
#'      a questionmark indicates that the expession is optional
#'      the dash is a escape to indicate that that is a real dot.
#' 
#'      will match the lines
#'      
#'              I bet I can spell better than you and george bush combined
#'              a bird in the nad is worth two george bushes
#'              
#'              
#' 
#' Metacharacters * and +         
#'
#'      indicate repetition; * means "any number including none of that item"
#'                           + means "at least one of that item"
#'                           
#' (.*)                           
#' 
#'      will match the lines with open and close parenthesis with 0 or more chars
#'      
#'              anyone wanna chat? (24, m, germany)
#'              hello, 20.m here... ( east area + drives + webcam)
#'              (he means older men)
#'              ()
#'              
#' [0-9]+ (.*)[0-9]+
#' 
#'      any possible combination of numbers separated by something other than
#'      numbers
#' 
#'      at least one number, then 0 or more of anything between parenthesis
#'      and at least one number
#'      
#'               working as MP here 729 battallion, 41nd birgade
#'               so say 2 or 3 years at colleage and 4 at uni makes us 23
#'               it went down on several occasons for like, 3 or 4 *days*
#'               Mmmm its time for me 2 go 2 bed
#'               
#' [Bb]ush( +[^ ]+ +){1,5} debate
#' 
#'      { and } are interval quantifiers;
#'      let us specify the minimum and maximum number of matches of an
#'      expression
#'      
#'      Bush as historically won all major debates he's done
#'      in my view, Bush doesn't need these debates..
#'      bush doesn't need the debates? maybe you are right
#'      That's what Bush supporters are doing about the debate.
#'      
#'      so it would start with bush higher or lowercase 
#'      end with debate
#'      and would have at least one space, followed by something that is 
#'      NOT an space and then followed by one space
#'      
#'      So while there is between one or 5 words between bush and debate
#'      
#' More metacharacters: and
#' 
#'      m,n     means at least m but not more than n matches
#'      m       means exactly m matches
#'      m,      means at least m matches
#'      
#' More metacharacters: (and) revisited
#' 
#'      in most implementations of regular expressions, the parenthesis
#'      not only limits the scope of the alternatives divided by | but can
#'      be used to remember text matched by the subexpression enclosed
#'      
#'      we can refer to the matched text with \1 \2 etc
#'      
#'      Example:
#'      
#'           +([a-zA-Z]+) +\1 +
#'          ^ this is suposed to be a space
#'           
#'           it would be at least one space followed by any 
#'           letter from a to z either mays or mins (being that variable 1)
#'           followed by an space and at least the variable 1 again
#'           followed at least by one space
#'           
#'           So generally it would be repetition of text "words" by spaces
#'           
#' The * is greedy so it always matches the LONGEST possible string that 
#' satisfies the regular expression
#' 
#'      ^s(.*)s
#'      
#'          it would be sentences starting with s followed with 0 or more of
#'          anything and ending with s
#'          
#'          sitting at starbucks
#'          setting up mysql and rails
#'          studying stuff for the exams
#'          spaghetti with marshmallows
#'          stop fighting with crackers
#'                                
#' The * greediness can be switched off with a question mark           
#'
#'      ^s(.*?)s$
#'      
#'          so this would be any sentence starting with s and ending with s
#'          with one or more of anything in the middle.










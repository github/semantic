import "hint" HLint.Default
import "hint" HLint.Dollar
import "hint" HLint.Generalise

ignore "Use mappend"
error "generalize ++" = (++) ==> (<>)
-- AMP fallout
error "generalize mapM"  = mapM  ==> traverse
error "generalize mapM_" = mapM_ ==> traverse_
error "generalize forM"  = forM  ==> for
error "generalize forM_" = forM_ ==> for_
error "Avoid return" =
    return ==> pure
    where note = "return is obsolete as of GHC 7.10"

error "use pure" = free . Pure ==> pure
error "use wrap" = free . Free ==> wrap

error "use extract" = headF . runCofree ==> extract

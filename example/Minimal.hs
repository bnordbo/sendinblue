module Mail.SendInBlue.Example.Minimal where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Text
import Mail.SendInBlue


minimal :: ApiKey -> EmailAddress -> IO Text
minimal key adr =
    runClient (mkEnv key)
        . send
        $ emailMessage (recipient  adr :| [])
                       (sender adr)
                       (htmlBody "<h1>Foo</h1>")

# sendinblue

Client for the SMTP API of the SendInBlue messaging platform.

## Usage

The following will create a minimal `EmailMessage` and send it using the
specified API key:

```haskell
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
```

For more advanced usage, see the documentation for `EmailMessage`.
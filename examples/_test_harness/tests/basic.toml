file="./main.tx3"

[[wallets]]
name = "bob"
balance = 10000000

[[wallets]]
name = "alice"
balance = 5000000

[[transactions]]
description = "bob sends 2 ada to alice"
template = "transfer"
signers = ["bob"]
args = { quantity = 2000000, sender = "@bob", receiver = "@alice" }

[[transactions]]
description = "alice sends 2 ada to bob"
template = "transfer"
signers = ["alice"]
args = { quantity = 2000000, sender = "@alice", receiver = "@bob" }

[[expect]]
from = "@bob"

[[expect.min_amount]]
amount = 9638899

[[expect]]
from = "@alice"

[[expect.min_amount]]
amount = 4638899

---------------------------- MODULE SimpleClock ---------------------------

VARIABLE clock

Init == clock \in {0, 1}

Tick == clock = 0 /\ clock' = 1

Tock == clock = 1 /\ clock' = 0

Next == Tick \/ Tock

Spec == Init /\ [][Next]_clock

=============================================================================
\* Modification History
\* Last modified Mon Jan 01 01:01:01 CET 1970 by ttaubert
\* Created Mon Jan 01 01:01:01 CEST 1970 by ttaubert


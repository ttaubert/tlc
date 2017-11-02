---------------------------- MODULE SimpleClock ---------------------------

VARIABLE clock

Init == clock \in {0, 1}

Tick == (*/\*) clock = 0
        /\ clock' = 1

Tock == (*/\*) clock = 1
        /\ clock' = 0

Next == Tick \/ Tock

Spec == Init /\ [][ Next ]_clock

=============================================================================
\* Modification History
\* Last modified Thu Nov 02 03:04:07 CET 2017 by ttaubert
\* Created Sat Oct 21 23:03:28 CEST 2017 by ttaubert


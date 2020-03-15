# Seamless Updates

## Context

You have a server (HW & SW) up and running; clients are connected to it; they
need constant connection to function; the server starts to smoke; you sense
it's the beginnings of a fire; you have to shut it down before you can put the
fire out and put it in some rice to boot it up again right after; before the
server burns down in flames you have just enough time to do updates; you take
the chance because you're never gonna get another like this one!

Updating SW is ezpz with hot code reload, so we'll consider that a solved
problem and only care about updates that require a HW reboot, e.g., OS packages
updates.

## Problem

How to update the server in a way the clients don't notice the server has
been/is being updated? The clients (the program itself) may know the server is
going down, but funcionality must not be interrupted, i.e., the user (someone
or something interacting with the client program) shouldn't be able to notice
it.

## (Idea on) How to (Possibly) Solve the Problem

Let's call the currently running server (HW&SW) that is on fire `current_srv`.
Let's assume we have another similar server (HW&SW) that is fully updated with
the latest OS packages and server SW, but is real nice and cool and not on fire
and ready to go! We'll call it `next_srv`.

### High Level

`current_srv` tells the clients "Yo, goin' away for a while".

"OHMYGAWD! What happened? Tell me everything!" say the clients.

"None o' yo' goddamn biz! But fret not, for I've a friend 'ere ready to
handle all'yer needs."

"Fine, if you're gonna be like that..."

(**_enter `next_srv`_**)

Here is where they part ways; the clients mind their own biz, start chatting
with `next_srv`, and a coupla messages later they completely forget about
`current_srv`.

However, it is not so easy for `current_srv`. It knows the relationships have
to end, but it still wants what's best for the clients, and so, in the
background, it shares all their secrets with `next_srv`.

`current_srv` can now finally take a nap and bathe in sweet white dry rice.

### With More Detail

Start `next_srv`. Tell `current_srv` it's time, how to contact `next_srv`, and
how clients can connect to `next_srv`. `current_srv` tells the clients to
connect to `next_srv`. The clients start connecting to `next_srv`. When a
client connects to `next_srv`, it tells `current_srv` it's done so. The client
doesn't start communicating with `next_srv` just yet. `current_srv` syncs with
`next_srv`, by sending it the current game state, and then tells the client to
start communicating with `next_srv`. Now the client should stop communicating
with `current_srv` and can disconnect from it. This syncing phase is where lag
may happen and state may be screwed, but this should be minimal, assuming
`current_srv` and `next_srv` have a fast connection between each other.

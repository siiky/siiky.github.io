% POSets
% siiky
% 2021/01/25

Take a look at my other page on [groups](groups.html) for the notation I'll be
using here.

The _transitive closure_ of a relation $\theta$, often represented as
$\theta^{*}$, is defined as:

$(a, b) \in \theta^{*} \iff \exists c \in S: (a, c) \in \theta \land (c, b) \in \theta^{*}$

# Interesting relations

Non-Reflexive
 ~ $\forall a \in S: (a, a) \notin \theta$

Anti-Symmetric
 ~ $\forall a, b \in S: (a, b) \in \theta \implies (b, a) \notin \theta$

Anti-Transitive
 ~ $\forall a, b, c \in S: (a, b) \in \theta \land (b, c) \in \theta \implies (a, c) \notin \theta$

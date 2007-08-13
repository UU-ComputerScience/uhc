-- mutual recursive data + type

type Y a = X a
data X a = X (Y a)

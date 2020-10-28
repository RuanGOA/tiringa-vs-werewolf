euclideanDistance :: (Int, Int) -> (Int, Int) -> (Int, Int, Float)
euclideanDistance (wx, wy) (tx, ty) = (wx, wy, sqrt (((wxf - txf) ** 2) + ((wyf - tyf) ** 2)))
                                      where wxf = fromInteger $ toInteger wx :: Float
                                            wyf = fromInteger $ toInteger wy :: Float
                                            txf = fromInteger $ toInteger tx :: Float
                                            tyf = fromInteger $ toInteger ty :: Float

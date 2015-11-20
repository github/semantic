module Console where

data Colour = Black | Red | Green | Yellow | Blue | Purple | Cyan | White
  deriving Bounded

instance Enum Colour where
  fromEnum Black = 30
  fromEnum Red = 31
  fromEnum Green = 32
  fromEnum Yellow = 33
  fromEnum Blue = 34
  fromEnum Purple = 35
  fromEnum Cyan = 36
  fromEnum White = 37

  toEnum 30 = Black
  toEnum 31 = Red
  toEnum 32 = Green
  toEnum 33 = Yellow
  toEnum 34 = Blue
  toEnum 35 = Purple
  toEnum 36 = Cyan
  toEnum 37 = White
  toEnum _ = error "unknown colour code"

data Style = Normal | Bold | Underline

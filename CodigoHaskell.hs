cont::Int->Int
cont 0=0
cont n= 1+ cont (div n 10)
suma::Int->Int
suma 0=0
suma n=(mod n 10)+suma(div n 10)

prod::Int->Int
prod 0=1
prod n=(mod n 10)*prod(div n 10)

resultado::Int->String
resultado 0=""
resultado n="numero de digitos:"++show(cont n)++" "++"la suma es:"++show(suma n)++" "++"el producto es:"++show(prod n)
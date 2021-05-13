!!!!!!! By Mohammad Jafari !!!!!!!!
program ThrowingMotion
IMPLICIT NONE
integer:: menuinput1 , menuinput2
real :: v0 , theta , t , answer , g = 9.81 , pi = 3.14159265359 , xans , yans , r 
PRINT*, "        Throwing motion ToolKit       "
print*, "          By Mohammad Jafari          "
print*, "                 Menu           "
print*, "            1 - Velocity        "
print*, "            2 - Placement       "
print*, "            3 - Time       "
read*, menuinput1
select case(menuinput1)
case(1)
print*, "                 Velocity           "
print*, "  1 - Velocity in specific t on x Component        "
print*, "  2 - Velocity in specific t on y Component            "
print*, "  3 - Velocity Vector in specific t             "

read*, menuinput2
select case(menuinput2)
case(1)
print*, "in ThrowingMotion Velocity on x Component is Constant"
print*, "V0 (m/s) : "
read*, v0
print*, "Angle (degree) : "
read*, theta
theta = theta*(pi/180)
answer = (v0 * cos(theta))
print*, "Answer is : " , Answer , "m/s"

case(2)
print*, "V0 (m/s) : "
read*, v0
print*, "Angle (degree) : "
read*, theta
theta = theta*(pi/180)
print*, "t : (s) "
read*, t
answer = (v0 * sin(theta)) - (g*t)
r = ((v0**2) * sin(2*theta)) / (g)
xans = ((v0 * cos(theta))*t)
if (xans > r) then
print*, "Answer is : " , "0"
ELSE
print*, "Answer is : " , answer , "m/s"
end if
case(3)
print*, "V0 (m/s) : "
read*, v0
print*, "Angle (degree) : "
read*, theta
theta = theta*(pi/180)
print*, "t : (s) "
read*, t
yans = (v0 * sin(theta)) - (g*t)
xans = (v0 * cos(theta))
r = ((v0**2) * sin(2*theta)) / (g)
xans = ((v0 * cos(theta))*t)
if (xans > r) then
print*, "Answer is : " , "0"
ELSE
answer = SQRT((yans**2)+(xans**2))
print*, "Answer is : " , answer , "m/s"
end if
case default
print*, "It's not A Valid Option !"
end select
case(2)
print*, "                 Velocity           "
print*, "  1 - Object Placement on y Component        "
print*, "  2 - Object Plaecment on x Component           "
print*, "  3 - Displacement Range of Object            "
print*, "  4 - Maximum Altitude of Object           "
read*, menuinput2
select case(menuinput2)
case(1)
print*, "V0 (m/s) : "
read*, v0
print*, "Angle (degree) : "
read*, theta
theta = theta*(pi/180)
print*, "t : (s) "
read*, t
r = ((v0**2) * sin(2*theta)) / (g)
xans = ((v0 * cos(theta))*t)
if (xans > r) then
print*, "Answer is : " , "0"
ELSE
answer = (v0 * sin(theta) * t ) - (0.5*g*(t**2))
print*, "Answer is : " , answer , "m"
end if

case(2)
print*, "V0 (m/s) : "
read*, v0
print*, "Angle (degree) : "
read*, theta
theta = theta*(pi/180)
print*, "t : (s) "
read*, t
answer = ((v0 * cos(theta))*t)
print*, "Answer is : " , Answer,"m"
case(3)
print*, "V0 (m/s) : "
read*, v0
print*, "Angle (degree) : "
read*, theta
theta = theta*(pi/180)
answer = ((v0**2) * sin(2*theta)) / (g)
print*, "Answer is : " , Answer , "m"
case(4)
print*, "V0 (m/s) : "
read*, v0
print*, "Angle (degree) : "
read*, theta
theta = theta*(pi/180)

answer = ((sin(theta)*v0)**2) / (2*g)
print*, theta
print*, "Answer is : " , Answer , "m"
case default
print*, "It's not A Valid Option !"
end select
case(3)
print*, "                 Time           "
print*, "  1 - Highest Altitude Moment        "
read*, menuinput2
select case(menuinput2)
case(1)
print*, "V0 (m/s) : "
read*, v0
print*, "Angle (degree) : "
read*, theta
theta = theta*(pi/180)
answer = (v0 * sin(theta))/(g)
print*, "Answer is : " , Answer , "Seconds After Throw"
case default
print*, "It's not A Valid Option !"
end select
case default
print*, "It's not A Valid Option !"
end select
end program
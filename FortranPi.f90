!Leibniz in Fortran
program Leibniz
    use iso_fortran_env
    implicit none

    !Timing variables
    integer :: start_time, end_time, clock_rate
    real :: elapsed_time

    !Main Program
    print *,"Starting Leibniz Approximation"
    call system_clock(start_time,clock_rate)
    print *, "Pi:", LeibnizTurbo(10000000)
    call system_clock(end_time)
    elapsed_time = real(end_time - start_time) / real(clock_rate)
    print *, "Elapsed Time:"
    write(*, '(F0.8)') elapsed_time

    contains

    !Leibniz function with optimizations
    real function LeibnizTurbo(iter)
        integer, intent(in) :: iter
        integer :: i
        real :: n, topterm, bottomterm, term
        n = 1.0
        topterm = -1.0
        do i = 2, iter-1
            bottomterm = i * 2
            term = topterm / (bottomterm - 1.0)
            n = n + term
            topterm = -topterm
        end do
        LeibnizTurbo = n * 4
    end function

end program Leibniz
module module_moss_biomass
    ! This demonstrative Fortran module tracks moss growth over time.
    ! Answer to the SE II Technical Interview at CGD, NCAR by Kuan-Chih Wang.
    !
    ! * Design principles
    !   1. Compliant with Fortran 2008 standard.
    !   2. OOP design.
    !   3. The states and parameters of each moss species are arranged into a derived type.
    !   4. Type-bound procedures are provided to correctly interact with each moss species.
    !
    ! * Usage
    !   See the `demo` program at the end of file for usage examples.
    !
    ! * Compilation
    !   $ cat /etc/os-release
    !   PRETTY_NAME="Ubuntu 22.04.1 LTS"
    !   NAME="Ubuntu"
    !   VERSION_ID="22.04"
    !   VERSION="22.04.1 LTS (Jammy Jellyfish)"
    !   ...
    !   $ gfortran --version
    !   GNU Fortran (Ubuntu 11.3.0-1ubuntu1~22.04) 11.3.0
    !   ...
    !   $ gfortran -o demo -std=f2008 -Wall -Wextra -Wpedantic module_moss_biomass.f90
    !
    ! * Run (Yes, it can actually run!)
    !   $ ./demo

    implicit none
    private
    public :: type_moss_biomass
    save

    type :: type_moss_biomass
        logical, private :: &
            initialized = .false.

        ! States of moss biomass
        real :: &
            b,       & ! Moss biomass
            f_light, & ! Moss growth response factor to light
            p          ! Moss productivity

        ! Species-specific parameters of moss biomass
        real :: &
            a_max,   & ! Maximum productivity per unit leaf area
            l_comp,  & ! Light compensation point
            l_sat,   & ! Light saturation point
            q,       & ! Parameter associated with moss respiration and turnover
            s_l        ! Moss-specific leaf area
    contains
        procedure :: &
            debug_dump_data, &
            moss_initialize, &
            moss_growth, &
            moss_growth_response_factor_to_light, &
            moss_productivity
    end type type_moss_biomass
contains
subroutine debug_dump_data(self)
    class(type_moss_biomass), intent(in) :: self

    if (.not. self%initialized) then
        print *, "Not yet initialized"
        stop 1
    end if

    print *, "b = ",       self%b
    print *, "f_light = ", self%f_light
    print *, "p = ",       self%p

    print *, "a_max = ",   self%a_max
    print *, "l_comp = ",  self%l_comp
    print *, "l_sat = ",   self%l_sat
    print *, "q = ",       self%q
    print *, "s_l = ",     self%s_l
end subroutine debug_dump_data

subroutine moss_initialize(self, b, a_max, l_comp, l_sat, q, s_l)
    class(type_moss_biomass), intent(inout) :: self

    real, intent(in) :: b, a_max, l_comp, l_sat, q, s_l

    if (self%initialized) then
        print *, "Already initialized"
        stop 1
    end if

    self%b       = b
    self%f_light = 0.0
    self%p       = 0.0

    self%a_max   = a_max
    self%l_comp  = l_comp
    self%l_sat   = l_sat
    self%q       = q
    self%s_l     = s_l

    self%initialized = .true.
end subroutine moss_initialize

subroutine moss_growth(self)
    class(type_moss_biomass), intent(inout) :: self

    if (.not. self%initialized) then
        print *, "Not yet initialized"
        stop 1
    end if

    call self%moss_productivity()

    self%b = self%b + self%p
end subroutine moss_growth

subroutine moss_productivity(self)
    class(type_moss_biomass), intent(inout) :: self

    if (.not. self%initialized) then
        print *, "Not yet initialized"
        stop 1
    end if

    call self%moss_growth_response_factor_to_light()

    self%p = self%s_l * self%a_max * self%f_light * self%b - self%b * self%q
end subroutine moss_productivity

subroutine moss_growth_response_factor_to_light(self)
    class(type_moss_biomass), intent(inout) :: self

    real :: lai_forest
    real :: lai_moss
    real, external :: get_lai_forest_from_elsewhere

    if (.not. self%initialized) then
        print *, "Not yet initialized"
        stop 1
    end if

    lai_forest = get_lai_forest_from_elsewhere()
    lai_moss = self%b * self%s_l

    self%f_light = (exp(-0.7 * (lai_forest + lai_moss)) - self%l_comp) / (self%l_sat - self%l_comp)
end subroutine moss_growth_response_factor_to_light
end module module_moss_biomass

function get_lai_forest_from_elsewhere() result(lai_forest)
    ! As per the instruction, "assuming you have access to forest LAI from elsewhere in the code".

    real :: lai_forest

    ! Return a reasonable value for demo purposes.
    lai_forest = 3.0
end function get_lai_forest_from_elsewhere

program demo
    ! A demo program that shows the module really works.

    use module_moss_biomass

    integer :: i

    ! Declare as many moss species as needed.
    type(type_moss_biomass) :: moss_species_1

    ! Initialize them by calling `moss_initialize` on each with different arguments.
    ! Positional arguments: b, a_max, l_comp, l_sat, q, s_l
    call moss_species_1%moss_initialize(0.1, 1.0, 0.1, 0.2, 0.001, 0.1)
    call moss_species_1%debug_dump_data()

    ! Advance timestep by calling `moss_growth`.
    ! Observe that the growth of this particular (non-existent) moss species levels out at around b ~= 2.75.
    do i = 1, 1000
        print *, "---------- Timestep = ", i, "----------"

        call moss_species_1%moss_growth()

        if (mod(i, 10) == 0) call moss_species_1%debug_dump_data()
    end do
end program demo

  program dynamo
    use lfric
    use psy
    use gaussian_quadrature_mod
    use num_dof_mod
    implicit none

    integer :: ncell, ndf, nlayer

    type(functionSpace) :: v3FunctionSpace, v0FunctionSpace
    type(field) :: pressureDensity

    call init_gauss()

    ncell = 9 ! three by three
    nlayer = 3
    write(*,'("Dynamo:Grid is ncell=",I2,":nlayer=",I2)') ncell,nlayer

    v3FunctionSpace = functionSpace(ncell,1) ! 1 = lowest order
    v0FunctionSpace = functionSpace(ncell,8) ! 8 is lowest order

    call num_dof_init(ncell,0)
    write(*,'("Dynamo:unique dofs:v0=",I3,":v2=",I3,":v3=",I3)') &
         nv0_g,nv2_g,nv3_g
    write(*,'("Dynamo:in 3d ndof = ",I3,"*",I3,"=",I3)') & 
         nv0_g,nlayer+1,nv0_g*(nlayer+1)
    

    pressureDensity = field(v3FunctionSpace,3)

    

    call invoke_RHS_V3(v3FunctionSpace)
    ! call invoke(RHS_V3(arg) )
    call test_integrate()
    call final_gauss()

  end program dynamo

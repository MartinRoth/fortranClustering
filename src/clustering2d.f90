! This subroutine is from https://github.com/lochbika/celltrack
subroutine clustering(data2d,startID,thres,finID,numIDs,tcl,missval,nx,ny)

  implicit none
  ! input
  real(kind=8), intent(in) :: data2d(nx,ny) ! the input 2D field
  integer, intent(in)      :: nx,ny         ! input dimensions
  integer, intent(in)      :: startID       ! the first ID to use
  real(kind=8), intent(in) :: thres         ! minimum value for clustering
  real(kind=8), intent(in) :: missval       ! value for missing data
  ! output
  integer, intent(out)     :: finID         ! the last used ID
  integer, intent(out)     :: numIDs        ! the total number of clusters/IDs
  integer, intent(out)     :: tcl(nx,ny)    ! the clustered field holding the IDs
  ! counters etc.
  integer                  :: x,y           ! for spatial dimenstions
  integer                  :: conx,cony     ! for spatial dims in nested loop
  integer                  :: i,tp          ! for misc.
  integer                  :: clID          ! for iterating clusters
  integer, allocatable     :: allIDs(:)     ! helper vector for re-assigning of IDs
  integer                  :: neighb(2)     ! temporary memory for neighbouring grid points
  logical                  :: mask(nx,ny)   ! logical field; true if value > thres

  ! initialize variables and arrays
  tcl=-999
  mask=.false.
  clID=startID
  numIDs=0

  ! mask values higher than threshold and if not missing value
  do y=1,ny
    do x=1,nx
      if(data2d(x,y)>thres .AND. data2d(x,y).ne.missval)mask(x,y)=.true.
    end do
  end do

  ! check if there are any gridpoints to cluster
  if(ANY(mask))then

    ! assign IDs to continous cells
    do y=1,ny
      do x=1,nx
        neighb=-999
        if(mask(x,y))then
          ! gather neighbouring IDs; left,up
          if(x.ne.1)  neighb(1)=tcl((x-1),y)
          if(y.ne.1)  neighb(2)=tcl(x,(y-1))
          ! check if there is NO cluster around the current pixel; create new one
          if(ALL(neighb==-999))then
            tcl(x,y)=clID
            clID=clID+1
            numIDs=numIDs+1
          else
            ! both neighbours are in the same cluster
            if(neighb(1)==neighb(2).and.neighb(1).ne.-999)then
              tcl(x,y)=neighb(1)
            end if
            ! both neighbors are in different clusters but none of them is (-999)
            if(neighb(1).ne.neighb(2) .and. neighb(1).ne.-999 .and. neighb(2).ne.-999)then
              numIDs=numIDs-1
              tcl(x,y)=MINVAL(neighb)
              ! update the existing higher cluster with the lowest neighbour
              do cony=1,ny
                do conx=1,nx
                  if(tcl(conx,cony)==MAXVAL(neighb))tcl(conx,cony)=MINVAL(neighb)
                end do
              end do
            end if
            ! both neighbors are in different clusters but ONE of them is empty(-999)
            if(neighb(1).ne.neighb(2) .and. (neighb(1)==-999 .or. neighb(2)==-999))then
              tcl(x,y)=MAXVAL(neighb)
            end if
          end if
        end if
      end do
    end do

    ! gather IDs and rename to gapless ascending IDs
    if(numIDs>0)then
      allocate(allIDs(numIDs))
      allIDs=-999
      clID=startID-1
      tp=1
      do y=1,ny
        do x=1,nx
          if(.NOT.ANY(allIDs==tcl(x,y)) .AND. tcl(x,y).ne.-999)then
            allIDs(tp)=tcl(x,y)
            tp=tp+1
          end if
        end do
      end do

      do i=1,tp-1
        clID=clID+1
        do y=1,ny
          do x=1,nx
            if(tcl(x,y)==allIDs(i))then
              tcl(x,y)=clID
            end if
          end do
        end do
      end do
      deallocate(allIDs)
    end if

  end if
  ! return final cluster ID
  finID=clID
end subroutine clustering

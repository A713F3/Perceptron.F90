module perceptron
implicit none

    real, dimension(2), private :: weights

contains
    function sigmoid(x)
    implicit none
        
        real :: x
        real :: sigmoid
    
        sigmoid = 1 / (1 + EXP(-1 * x))
    
    end function sigmoid

    function relu(x)
    implicit none

        real :: x
        real :: relu

        relu = MAX(x, 0.0)

    end function relu

    function think(x, y) result (output)
        real :: x, y, output

        output = weights(1)*x + weights(2)*y
        ! output = sigmoid(output)

        if (output >= 0.5) then
            output = 1.0
        else
            output = 0.0
        end if

    end function think

    subroutine train(input, labels, epochs, learning_rate)
        real, dimension(:, :), intent(in) :: input
        real, dimension(:), intent(in) :: labels

        integer :: asize, i, j, epochs
        real :: output, error, learning_rate

        asize = size(input)

        call RANDOM_NUMBER(weights)

        do i = 1, epochs
            do j = 1, asize
                output = think(input(j, 1), input(j, 2))
                error = labels(j) - output
                
                weights(1) = weights(1) + error * learning_rate
                weights(2) = weights(2) + error * learning_rate
            end do
        end do

    end subroutine train

    function evaluate(input, labels) result(mse)
        real, dimension(:, :), intent(in) :: input
        real, dimension(:), intent(in) :: labels
        real :: mse, output, error
        integer :: asize, i

        asize = size(input)
        mse = 0.0

        do i = 1, asize
            output = think(input(i, 1), input(i, 2))
            error = labels(i) - output

            mse = mse + (error ** 2)
        end do

        mse = mse / asize

    end function evaluate

end module perceptron

program perceptron_example
use perceptron 
implicit none
    real, dimension(4, 2) :: inputs
    real, dimension(4) :: labels
    real :: mse_before, mse_after
    integer :: i

    ! inputs and labels for and gate
    inputs(1, 1) = 0.0
    inputs(1, 2) = 0.0
    inputs(2, 1) = 0.0
    inputs(2, 2) = 1.0
    inputs(3, 1) = 1.0
    inputs(3, 2) = 0.0
    inputs(4, 1) = 1.0
    inputs(4, 2) = 1.0

    labels(1) = 0.0
    labels(2) = 0.0
    labels(3) = 0.0
    labels(4) = 1.0

    mse_before = evaluate(inputs, labels)

    print*, '-Before'
    do i = 1, 4
        print*, 'Label', i, ': ', labels(i), 'Output: ', think(inputs(i, 1), inputs(i, 2))
    end do

    call train(inputs, labels, 1000, 0.001)

    mse_after = evaluate(inputs, labels)

    print*, '-After'
    do i = 1, 4
        print*, 'Label', i, ': ', labels(i), 'Output: ', think(inputs(i, 1), inputs(i, 2))
    end do

    print*, "Mean Square Error Before: ", mse_before
    print*, "Mean Square Error After: ", mse_after


end program perceptron_example


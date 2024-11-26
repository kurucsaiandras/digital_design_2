-- -----------------------------------------------------------------------------
--
--  Title      :  Edge-Detection design project - task 2.
--             :
--  Developers :  Andras - s??????@student.dtu.dk 
--             :  Rene - s??????@student.dtu.dk
--             :
--  Purpose    :  This design contains an entity for the accelerator that must be built
--             :  in task two of the Edge Detection design project. It contains an
--             :  architecture skeleton for the entity as well.
--             :
--  Revision   :  1.0   ??-??-??     Final version
--             :
--
-- -----------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- The entity for task two. Notice the additional signals for the memory.
-- Reset is active high.
--------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.types.all;

entity acc is
    port(
        clk    : in  bit_t;             -- The clock.
        reset  : in  bit_t;             -- The reset signal. Active high.
        addr   : out halfword_t;        -- Address bus for data.
        dataR  : in  word_t;            -- The data bus for reading.
        dataW  : out word_t;            -- The data bus for writing.
        en     : out bit_t;             -- Request signal for data.
        we     : out bit_t;             -- Read/Write signal for data.
        start  : in  bit_t;
        finish : out bit_t
    );
end acc;

--------------------------------------------------------------------------------
-- The description of the accelerator.
--------------------------------------------------------------------------------

architecture rtl of acc is

    -- All internal signals are defined here
    type state_type is (
        I1, I2, I3, I4, I5, I6, S1_init, S2_init, S3_init, S4_init,
        S1_pre, S0, S1, S2, F
    ); -- Added S_WRITE state

    signal state, next_state          : state_type;
    signal row_1, next_row_1          : std_logic_vector(63 downto 0) := (others => '0');
    signal row_2, next_row_2          : std_logic_vector(71 downto 0) := (others => '0');
    signal row_3, next_row_3          : std_logic_vector(79 downto 0) := (others => '0');
    signal curr_write_buff, next_write_buff : word_t;
    
    -- For the first sobel sum
    signal dx, dy                     : signed(11 downto 0);   -- 12-bit signed for intermediate calculations
    signal abs_dx, abs_dy             : std_logic_vector(11 downto 0); -- 12-bit unsigned for absolute values
    signal sobel_sum                  : std_logic_vector(11 downto 0); -- 12-bit unsigned for sum of gradients
    signal result                     : std_logic_vector(7 downto 0);  -- 8-bit output
    
    -- For the second sobel sum
    signal dx2, dy2                     : signed(11 downto 0);   -- 12-bit signed for intermediate calculations
    signal abs_dx2, abs_dy2             : std_logic_vector(11 downto 0); -- 12-bit unsigned for absolute values
    signal sobel_sum2                   : std_logic_vector(11 downto 0); -- 12-bit unsigned for sum of gradients
    signal result2                      : std_logic_vector(7 downto 0);  -- 8-bit output
        
        

    -- Control signals
    signal curr_en : std_logic;
    signal curr_we : std_logic;
    signal addr_read  : halfword_t;
    signal addr_write : halfword_t;
    signal curr_addr                        : halfword_t;
    signal curr_addr_ptr, next_addr_ptr     : std_logic_vector(15 downto 0);


    component three_dual_one_clock
        port (
            clk    : in  std_logic;
            ena    : in  std_logic;                    
            enb    : in  std_logic;                    
            wea    : in  std_logic;                    
            addra  : in  std_logic_vector(7 downto 0); 
            addrb  : in  std_logic_vector(7 downto 0); 
            tag_offset   : in std_logic_vector(1 downto 0);
            dia    : in  std_logic_vector(31 downto 0); 
            dob1   : out std_logic_vector(31 downto 0); 
            dob2   : out std_logic_vector(31 downto 0); 
            dob3   : out std_logic_vector(31 downto 0)  
        );
    end component;
    
    
        -- Signals to connect to the component ports
    signal bram_en_write        : std_logic;
    signal bram_en_read         : std_logic;
    signal bram_we_write        : std_logic;
    signal curr_bram_addr_ptr, next_bram_addr_ptr       : std_logic_vector(7 downto 0);
    signal bram_addr_read       : std_logic_vector(7 downto 0);
    signal bram_addr_write      : std_logic_vector(7 downto 0);
    signal tag_offset           : std_logic_vector(1 downto 0);
    signal bram_read_row1        : std_logic_vector(31 downto 0);
    signal bram_read_row2        : std_logic_vector(31 downto 0);
    signal bram_read_row3        : std_logic_vector(31 downto 0);
    signal bram_write                                 : std_logic_vector(31 downto 0);
    signal curr_init_bram, next_init_bram             : std_logic_vector(1 downto 0);
    
    signal curr_counter, next_counter : std_logic_vector(1 downto 0);
begin   

    -- Instantiate the three_dual_one_clock component
    ram_inst: three_dual_one_clock
        port map (
            clk       => clk,
            ena       => bram_en_write,
            enb       => bram_en_read,
            wea       => bram_we_write,
            addra     => bram_addr_write,
            addrb     => bram_addr_read,
            tag_offset=> tag_offset,
            dia       => bram_write,
            dob1      => bram_read_row1,
            dob2      => bram_read_row2,
            dob3      => bram_read_row3
        );


    -- Output assignments
    en <= curr_en;
    we <= curr_we; 
    addr <= curr_addr;

-- Combinational process for Dx, Dy, and Sobel filter computation
cl_sobel: process(row_1, row_2, row_3, dx, dy, abs_dx, abs_dy, sobel_sum, dx2, dy2, abs_dx2, abs_dy2, sobel_sum2)
begin
    -- Existing Sobel computation for result
    -- Calculate horizontal gradient (Dx)
    dx <= signed(x"0" & row_1(7 downto 0))                  -- s13
           - signed(x"0" & row_1(23 downto 16))             -- s11
           + signed(unsigned(x"0" & row_2(7 downto 0)) sll 1) -- s23 * 2
           - signed(unsigned(x"0" & row_2(23 downto 16)) sll 1) -- s21 * 2
           + signed(x"0" & row_3(7 downto 0))               -- s33
           - signed(x"0" & row_3(23 downto 16));            -- s31

    -- Calculate vertical gradient (Dy)
    dy <= signed(x"0" & row_1(23 downto 16))               -- s11
           - signed(x"0" & row_3(23 downto 16))             -- s31
           + signed(unsigned(x"0" & row_1(15 downto 8)) sll 1) -- s12 * 2
           - signed(unsigned(x"0" & row_3(15 downto 8)) sll 1) -- s32 * 2
           + signed(x"0" & row_1(7 downto 0))               -- s13
           - signed(x"0" & row_3(7 downto 0));              -- s33

    -- Compute absolute values and sum them
    abs_dx <= std_logic_vector(abs(dx));
    abs_dy <= std_logic_vector(abs(dy));

    -- Sum of absolute gradients
    sobel_sum <= std_logic_vector(unsigned(abs_dx) + unsigned(abs_dy));

    -- Scale the result to 8 bits
    result <= sobel_sum(10 downto 3);

    -- New Sobel computation for result2 with shifted indices
    -- Calculate horizontal gradient (Dx2)
    dx2 <= signed(x"0" & row_1(15 downto 8))                 -- s13 shifted by 8 bits
            - signed(x"0" & row_1(31 downto 24))             -- s11 shifted by 8 bits
            + signed(unsigned(x"0" & row_2(15 downto 8)) sll 1) -- s23 * 2 shifted
            - signed(unsigned(x"0" & row_2(31 downto 24)) sll 1) -- s21 * 2 shifted
            + signed(x"0" & row_3(15 downto 8))              -- s33 shifted by 8 bits
            - signed(x"0" & row_3(31 downto 24));            -- s31 shifted by 8 bits

    -- Calculate vertical gradient (Dy2)
    dy2 <= signed(x"0" & row_1(31 downto 24))               -- s11 shifted by 8 bits
            - signed(x"0" & row_3(31 downto 24))             -- s31 shifted by 8 bits
            + signed(unsigned(x"0" & row_1(23 downto 16)) sll 1) -- s12 * 2 shifted
            - signed(unsigned(x"0" & row_3(23 downto 16)) sll 1) -- s32 * 2 shifted
            + signed(x"0" & row_1(15 downto 8))              -- s13 shifted by 8 bits
            - signed(x"0" & row_3(15 downto 8));             -- s33 shifted by 8 bits

    -- Compute absolute values and sum them
    abs_dx2 <= std_logic_vector(abs(dx2));
    abs_dy2 <= std_logic_vector(abs(dy2));

    -- Sum of absolute gradients
    sobel_sum2 <= std_logic_vector(unsigned(abs_dx2) + unsigned(abs_dy2));

    -- Scale the result2 to 8 bits
    result2 <= sobel_sum2(10 downto 3);
    
end process cl_sobel;




    cl_addr: process(curr_en, curr_we, addr_write, addr_read)    
    begin
    -- Update current address based on we and en signals
    if (curr_en = '1' and curr_we = '1') then
        curr_addr <= addr_write;
    elsif (curr_en = '1' and curr_we = '0') then
        curr_addr <= addr_read;
    else
        curr_addr <= (others => '0');
    end if;
    end process cl_addr;




    -- Combinational logic for state transitions and output logic
    cl_states: process(
        start, state, curr_addr_ptr, curr_write_buff,
        addr_write, addr_read, row_1, row_2, row_3, result, curr_counter, bram_read_row1, bram_read_row2
    )
        constant row_2_offs  : integer := 88;
        constant row_3_offs  : integer := 176;
        constant write_offs  : integer := 25344;
    begin

        -- Default assignments
        next_counter <= curr_counter;
        next_write_buff <= curr_write_buff;
        next_state      <= state;
        next_addr_ptr   <= curr_addr_ptr;
        next_bram_addr_ptr <= curr_bram_addr_ptr;
        next_row_1      <= row_1;
        next_row_2      <= row_2;
        next_row_3      <= row_3;
        finish          <= '0';
        dataW <= (others => '0');
        ------------------------------ BRAM -------------------------------
        bram_we_write <= '0';
        bram_en_write <= '0';
        bram_en_read <= '0';
        -------------------------------------------------------------------
        
        
        case state is
            -- S0: Idle
            when S0 =>
                curr_en         <= '1';
                curr_we         <= '0';
                next_write_buff <= (others => '0');
                next_addr_ptr   <= (others => '0');
                addr_read  <= (others => '0');
                addr_write <= (others => '0');
                if start = '1' then
                    ------------------------------ BRAM -------------------------------
                    -- Instantiate BRAM for writing
                    bram_we_write <= '1';
                    bram_en_write <= '1';
                    bram_en_read  <= '0';
                     -- Instantiate the counter for tag offsets
                    next_counter <= (others => '0');
                    -- Instantiate reading and writing address'
                    bram_addr_write <= (others => '0');
                    -- Also setting up address location for the read in next state (1st row)
                    bram_addr_read <= (others => '0');
                    -- Used for reading and writing at the correct offset
                    next_bram_addr_ptr <= (others => '0');
                    -------------------------------------------------------------------
                    next_state      <= I1;
                end if;
                
            -- Initialization states (I1 to I7): For setting up the shift registers
            
            when I1 =>
                -- Reading the data from the 1st row address location
                next_row_1(31 downto 0) <= dataR;
                curr_en         <= '1';
                curr_we         <= '0';
                bram_we_write   <= '1';
                bram_en_write   <= '1';
                -- Setting up address location for the read in next state (2nd row) 
                addr_read  <= std_logic_vector(unsigned(curr_addr_ptr) + row_2_offs);
                ------------------------------ BRAM -------------------------------
                -- To ensure dataR is stored in row1 (assuming we initialize at "00")
                bram_write <= dataR;
                tag_offset <= "10"; 
                -------------------------------------------------------------------
                next_state      <= I2;
            
            when I2 =>
                -- Reading the data from the 2nd row address location
                next_row_2(31 downto 0) <= dataR;
                curr_en         <= '1';
                curr_we         <= '0';
                bram_we_write   <= '1';
                bram_en_write   <= '1';
                -- Setting up address location for the read in next state (3rd row)
                addr_read  <= std_logic_vector(unsigned(curr_addr_ptr) + row_3_offs);
                -- Incrementing the address pointer 
                next_addr_ptr   <= std_logic_vector(unsigned(curr_addr_ptr) + 1);
                ------------------------------ BRAM -------------------------------
                -- To ensure dataR is stored in row2 (assuming we initialize at "00")
                bram_write <= dataR;
                tag_offset <= "01";  
                -------------------------------------------------------------------
                next_state      <= I3;
                

            when I3 =>
                -- Reading the data from the 3rd row address location
                next_row_3(31 downto 0) <= dataR;
                curr_en         <= '1';
                curr_we         <= '0';
                bram_we_write   <= '1';
                bram_en_write   <= '1';
                -- Updating the read address location after iterating the address pointer to the next word (i.e. next 4 pixels)
                -- Setting up address location for the read in next state (1st row)
                addr_read  <= curr_addr_ptr;
                ------------------------------ BRAM -------------------------------
                -- Incrementing the BRAM pointer to write the next word of pixels (i.e. next 4 pixels) at an empty adjacent word location
                next_bram_addr_ptr <= std_logic_vector(unsigned(curr_bram_addr_ptr) + 1);
                -- To ensure dataR is stored in row3 (assuming we initialize at "00")
                bram_write <= dataR;
                tag_offset <= "00";
                -------------------------------------------------------------------
                next_state      <= I4;

            when I4 =>
                -- Reading the data from the 1st row address location and have it buffered
                next_row_1(63 downto 32) <= dataR;
                curr_en         <= '1';
                curr_we         <= '0';
                bram_we_write   <= '1';
                bram_en_write   <= '1';
                -- Setting up address location for the read in next state (2nd row)
                addr_read  <= std_logic_vector(unsigned(curr_addr_ptr) + row_2_offs);
                ------------------------------ BRAM -------------------------------
                -- Updating the BRAM write address location after iterating the BRAM address pointer to point to the next adjacent word (i.e. next 4 pixels) location
                bram_addr_write <= curr_bram_addr_ptr; 
                -- To ensure buffered dataR is stored in row1 (assuming we initialize at "00")
                bram_write <= dataR;
                tag_offset <= "10";
                -------------------------------------------------------------------
                next_state      <= I5;

            when I5 =>
                -- Reading the data from the 2nd row address location and have it buffered
                next_row_2(63 downto 32) <= dataR;
                curr_en         <= '1';
                curr_we         <= '0';
                bram_we_write   <= '1';
                bram_en_write   <= '1';
                -- Setting up address location for the read in next state (3rd row)
                addr_read  <= std_logic_vector(unsigned(curr_addr_ptr) + row_3_offs);
                -- Incrementing the address pointer 
                next_addr_ptr   <= std_logic_vector(unsigned(curr_addr_ptr) + 1);
                ------------------------------ BRAM -------------------------------
                -- To ensure buffered dataR is stored in row2 (assuming we initialize at "00")
                bram_write <= dataR;
                tag_offset <= "01";
                -------------------------------------------------------------------
                next_state      <= I6;

            when I6 =>
                -- Reading the data from the 3rd row address location and have it buffered
                next_row_3(63 downto 32) <= dataR;
                curr_en         <= '1';
                curr_we         <= '0';
                bram_we_write   <= '1';
                bram_en_write   <= '1';
                -- No setting up address location for the read in next state since the ordering is changed up
                --
                ------------------------------ BRAM -------------------------------
                -- Incrementing the BRAM pointer to write the next word of pixels (i.e. next 4 pixels) at an empty adjacent word location
                next_bram_addr_ptr <= std_logic_vector(unsigned(curr_bram_addr_ptr) + 1);
                -- To ensure buffered dataR is stored in row3 (assuming we initialize at "00")
                bram_write <= dataR;
                tag_offset <= "00"; -- Due to init.: third row. 
                -------------------------------------------------------------------
                next_state      <= S1_init;


            -- Processing states (S1 to S4), assembling write buffer
            when S1_init =>
                -- Check for end of processing (i.e. check if the address points past the last pixel location)
                -- The + 1 is added since the address pointer is 1 increment in front of the pixel calculations due to the buffer 
                if (unsigned(curr_addr_ptr) + row_3_offs > write_offs + 1) then
                    next_state <= F;
                else
                    curr_en        <= '1';
                    curr_we        <= '0';
                    bram_we_write  <= '1';
                    bram_en_write  <= '1';
                    -- Setting up address location for the read in next state (3rd row)
                    addr_read <= std_logic_vector(unsigned(curr_addr_ptr) + row_3_offs);
                    -- Shift rows by 1 pixel
                    next_row_1 <= std_logic_vector(unsigned(row_1) srl 8);
                    next_row_2 <= std_logic_vector(unsigned(row_2) srl 8);
                    next_row_3 <= std_logic_vector(unsigned(row_3) srl 8);
                    -- Loading the result for the 2nd pixel of result-buffer. This is due to the first pixel possibly being boundary
                    next_write_buff(15 downto 8) <= result;
                    ------------------------------ BRAM -------------------------------
                    -- Updating the BRAM write address location after iterating the BRAM address pointer to point to the next adjacent word (i.e. next 4 pixels) location
                    bram_addr_write <= curr_bram_addr_ptr;
                    -------------------------------------------------------------------
                    next_state <= S2_init;    
                end if;

            when S2_init =>
                curr_en        <= '1';
                curr_we        <= '0';
                bram_we_write  <= '1';
                bram_en_write  <= '1';
                -- Setting up address location for the read in next state (2nd row)
                addr_read <= std_logic_vector(unsigned(curr_addr_ptr) + row_2_offs);
                -- Shift rows by 1 pixel
                next_row_1 <= std_logic_vector(unsigned(row_1) srl 8);
                next_row_2 <= std_logic_vector(unsigned(row_2) srl 8);
                next_row_3 <= std_logic_vector(unsigned(row_3) srl 8);
                -- Reading the data from the 3rd row address location
                next_row_3(79 downto 48) <= dataR;
                -- Loading the result for the 3rd pixel of result-buffer. This is due to the first pixel possibly being boundary
                next_write_buff(23 downto 16) <= result;
                ------------------------------ BRAM -------------------------------
                -- To ensure buffered dataR is stored in row3 (assuming we initialize at "00")
                if unsigned(curr_addr_ptr) > 87 then
                    bram_write <= dataR;
                    tag_offset <= curr_counter;
                else
                    bram_write <= dataR;
                    tag_offset <= "00";
                end if;
                -------------------------------------------------------------------

                next_state <= S3_init;

            when S3_init =>
                -- Checks if the address pointer to the current calculations was at the boundaries
                -- Loading the result for the 4th pixel of result-buffer. This is due to the first pixel possibly being boundary
                if ((unsigned(curr_addr_ptr)-1) mod 88) = 0 then 
                    next_write_buff(31 downto 24) <= "00000000";
                else
                    next_write_buff(31 downto 24) <= result;
                end if; 
                curr_en        <= '1';
                curr_we        <= '0';
                bram_we_write  <= '1';
                bram_en_write  <= '1';
                -- Setting up address location for the read in next state (1st row)
                addr_read <= curr_addr_ptr;
                -- Shift rows by 1 pixel
                next_row_1 <= std_logic_vector(unsigned(row_1) srl 8);
                next_row_2 <= std_logic_vector(unsigned(row_2) srl 8);
                next_row_3 <= std_logic_vector(unsigned(row_3) srl 8);
                -- Reading the data from the 2rd row address location
                next_row_2(71 downto 40) <= dataR;
                ------------------------------ BRAM -------------------------------
                -- To ensure buffered dataR is stored in row2 (assuming we initialize at "00")
                if unsigned(curr_addr_ptr) > 87 then
                    -- Do not do anything
                else
                    bram_write <= dataR;
                    tag_offset <= "01";
                end if;
                -------------------------------------------------------------------
                next_state <= S4_init;
                
            when S4_init =>
                -- Shift rows
                curr_en        <= '1';
                curr_we        <= '1';
                bram_we_write  <= '1';
                bram_en_write  <= '1';
                -- Check if the write values are circling onto a new line
                if unsigned(curr_addr_ptr) = 87 then
                    ------------------------------ BRAM -------------------------------
                    bram_en_read   <= '1';
                    -- Reset the address pointer for the writing and reading
                    next_bram_addr_ptr <= (others => '0');
                    -- To ensure buffered dataR is stored in row1 (assuming we initialize at "00")
                    bram_write <= dataR;
                    tag_offset <= "10";
                    next_counter <= std_logic_vector(unsigned(curr_counter) + 2);
                    -------------------------------------------------------------------
                else
                    ------------------------------ BRAM -------------------------------
                    -- Incrementing the BRAM pointer to write the next word of pixels (i.e. next 4 pixels) at an empty adjacent word location
                    if ((unsigned(curr_addr_ptr)-1) mod 88) = 0 then 
                        -- Do nothing
                    else
                        next_bram_addr_ptr <= std_logic_vector(unsigned(curr_bram_addr_ptr) + 1); 
                    end if;
                        
                    
                    if unsigned(curr_addr_ptr) > 87 then
                        -- Do nothing
                    else
                        -- To ensure buffered dataR is stored in row1 (assuming we initialize at "00")
                        bram_write <= dataR;
                        tag_offset <= "10";
                    end if;
                    -------------------------------------------------------------------
                end if;
                
                -- Check for the boundary conditions
                if ((unsigned(curr_addr_ptr)-1) mod 88) = 0 then 
                    next_write_buff(7 downto 0) <= "00000000";
                    next_addr_ptr  <= std_logic_vector(unsigned(curr_addr_ptr) + 1);
                    next_state <= S1_pre;
                else
                    next_write_buff(7 downto 0) <= result;
                   -- Incrementing the address pointer 
                    next_addr_ptr  <= std_logic_vector(unsigned(curr_addr_ptr) + 1);
                    next_state <= S1_init;
                end if;
                -- Shift rows by 1 pixel
                next_row_1 <= std_logic_vector(unsigned(row_1) srl 8);
                next_row_2 <= std_logic_vector(unsigned(row_2) srl 8);
                next_row_3 <= std_logic_vector(unsigned(row_3) srl 8);
                -- Reading the data from the 3rd row address location
                next_row_1(63 downto 32) <= dataR;
                -- Setting up address location for the write happening in the state transition
                addr_write <= std_logic_vector(unsigned(curr_addr_ptr) + write_offs - 2 + row_2_offs);
                
                dataW <= curr_write_buff;

               
            
            when S1_pre =>
                -- Check if it is the first or last pixel of the line, if so it turns 0
                -- The minus 1 offset is due to the address pointer being one word ahead
                -- Loading the result for the 2nd pixel of result-buffer. This is due to the first pixel possibly being boundary
                next_write_buff(15 downto 8) <= result;
                curr_en        <= '1';
                curr_we        <= '0';
                bram_we_write  <= '1';
                bram_en_write  <= '0';
                bram_en_read   <= '1';
                -- Setting up address location for the read in next state (3rd row)
                -- This will be the only row read from at this point forward
                addr_read <= std_logic_vector(unsigned(curr_addr_ptr) + row_3_offs);
                -- Shift rows by 1 pixel
                next_row_1 <= std_logic_vector(unsigned(row_1) srl 8);
                next_row_2 <= std_logic_vector(unsigned(row_2) srl 8);
                next_row_3 <= std_logic_vector(unsigned(row_3) srl 8);
                ------------------------------ BRAM -------------------------------
                -- Updating the BRAM write address location after iterating the BRAM address pointer to point to the next adjacent word (i.e. next 4 pixels) location
                
                bram_addr_read <= curr_bram_addr_ptr;
                bram_addr_write <= curr_bram_addr_ptr;
                -------------------------------------------------------------------
                next_state <= S1;    
            
            
            
            
            
            -------------------------------------------- SPEEDUP ---------------------------------------------
            -- Now two rows wil always be buffered, which means only one row will be read from
            -- The shifting window is now buffered in memory, which allows 2 pixels to be calculated per. clock cycle
            -- S1 read pixels from main memory (2 computations)
            -- S2 write pixels into main memory (2 computations)
            --------------------------------------------------------------------------------------------------
            when S1 => -- Check for end of processing
                if (unsigned(curr_addr_ptr) + row_3_offs = write_offs + 2) then
                    next_state <= F;
                else
                    curr_en        <= '1';
                    curr_we        <= '0';
                    bram_we_write  <= '1';
                    bram_en_write  <= '1';
                    bram_en_read   <= '1';
                    -- If the BRAM pointer reaches 87, i.e. end of line
                    if ((unsigned(curr_bram_addr_ptr) + 1) mod 88) = 0 then 
                        next_bram_addr_ptr <= (others => '0');
                        if unsigned(curr_counter) = 0 then  -- Compare as unsigned
                            next_counter <= std_logic_vector(unsigned(curr_counter) + 2);  -- Reset to 0
                        else
                            next_counter <= std_logic_vector(unsigned(curr_counter) - 1);  -- Increment
                        end if;
                    else
                        -- Update the BRAM address pointer
                        next_bram_addr_ptr <= std_logic_vector(unsigned(curr_bram_addr_ptr) + 1);
                    end if;
                    
                    -- Check for the boundary conditions
                    if ((unsigned(curr_addr_ptr)-1) mod 88) = 0 then 
                        -- Loading the result for the 3rd and 4th pixel of result-buffer
                        next_write_buff(31 downto 16) <= "00000000" & result;
                    else
                        next_write_buff(31 downto 16) <= result2 & result;             
                    end if;
                    -- This will be the only row read from at this point forward
                    addr_read <= std_logic_vector(unsigned(curr_addr_ptr) + row_3_offs);
                    -- Shift rows by 2 pixels
                    next_row_1 <= std_logic_vector(unsigned(row_1) srl 16);
                    next_row_2 <= std_logic_vector(unsigned(row_2) srl 16);
                    next_row_3 <= std_logic_vector(unsigned(row_3) srl 16);
                    
                    ------------------------------ BRAM -------------------------------
                    -- Will start at the first row and iterate to the next rows
                    tag_offset <= curr_counter;
                    
                    
                    if unsigned(curr_bram_addr_ptr) = 87 then
                        bram_addr_read <= (others => '0');
                    else
                        bram_addr_read <= std_logic_vector(unsigned(curr_bram_addr_ptr) + 1);
                    end if;
                    
                    
                    -------------------------------------------------------------------  
                    next_state <= S2;
                end if;
                
            
            when S2 =>
                curr_en        <= '1';
                curr_we        <= '1';
                bram_we_write  <= '1';
                bram_en_write  <= '1';
                bram_en_read   <= '1';
                -- Check for the boundary conditions
                if ((unsigned(curr_addr_ptr)-1) mod 88) = 0 then -- Check when we reach a new line
                    next_write_buff(15 downto 0) <=  result2 & "00000000";
                else 
                    next_write_buff(15 downto 0) <=  result2 & result;
                end if;
                
                -- Will start at the first row and iterate to the next rows
                tag_offset <= curr_counter;
                -- Shift rows by 2 pixels
                next_row_1 <= std_logic_vector(unsigned(row_1) srl 16);
                next_row_2 <= std_logic_vector(unsigned(row_2) srl 16);
                next_row_3 <= std_logic_vector(unsigned(row_3) srl 16);
                next_row_1(55 downto 24) <= bram_read_row1;
                next_row_2(55 downto 24) <= bram_read_row2;
                next_row_3(55 downto 24) <= dataR;
                -- Setting up address location for the write in happening in the state transition
                addr_write <= std_logic_vector(unsigned(curr_addr_ptr) + write_offs - 2 + row_2_offs);
                -- Incrementing the address pointer 
                next_addr_ptr  <= std_logic_vector(unsigned(curr_addr_ptr) + 1);
                ------------------------------ BRAM -------------------------------
                -- Updating the BRAM write address location after iterating the BRAM address pointer to point to the next adjacent word (i.e. next 4 pixels) location
                bram_addr_write <= curr_bram_addr_ptr;
                -- To ensure buffered dataR is stored in row2 (assuming we initialize at "00")
                bram_write <= dataR;
                tag_offset <= curr_counter;
                -------------------------------------------------------------------
                dataW <= curr_write_buff;
                next_state <= S1;
            
            -- Finish state
            when F =>
                finish <= '1';
                if start = '1' then
                    next_state <= F;
                else
                    next_state <= S0;
                end if;

            -- Default case
            when others =>
                next_state <= S0;
        end case;

    end process cl_states;

    -- Sequential process to update state and signals
    myprocess: process(clk, curr_en, curr_we)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                -- Initialize signals on reset
                state           <= S0;
                curr_addr_ptr   <= (others => '0');
                curr_write_buff <= (others => '0');
                row_1           <= (others => '0');
                row_2           <= (others => '0');
                row_3           <= (others => '0');
                -- Block mem --
                curr_bram_addr_ptr <= (others => '0');
                curr_counter <= (others => '0');
                -- --------- --
            else
                -- Update current state and signals
                state           <= next_state;
                curr_addr_ptr   <= next_addr_ptr;
                row_1           <= next_row_1;
                row_2           <= next_row_2;
                row_3           <= next_row_3;
                curr_write_buff <= next_write_buff;
                -- Block mem --
                curr_counter <= next_counter;
                curr_bram_addr_ptr <= next_bram_addr_ptr;
                -- --------- --
                
            end if;
        end if;
    end process myprocess;

end rtl;
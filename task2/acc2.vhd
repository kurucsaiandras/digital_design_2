-- -----------------------------------------------------------------------------
--
--  Title      :  Edge-Detection design project - task 2.
--             :
--  Developers :  YOUR NAME HERE - s??????@student.dtu.dk
--             :  YOUR NAME HERE - s??????@student.dtu.dk
--             :
--  Purpose    :  This design contains an entity for the accelerator that must be build
--             :  in task two of the Edge Detection design project. It contains an
--             :  architecture skeleton for the entity as well.
--             :
--  Revision   :  1.0   ??-??-??     Final version
--             :
--
-- -----------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- The entity for task two. Notice the additional signals for the memory.
-- reset is active high.
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
        dataR  : in  word_t;            -- The data bus.
        dataW  : out word_t;            -- The data bus.
        en     : out bit_t;             -- Request signal for data.
        we     : out bit_t;             -- Read/Write signal for data.
        start  : in  bit_t;
        finish : out bit_t
    );
end acc;

--------------------------------------------------------------------------------
-- The desription of the accelerator.
--------------------------------------------------------------------------------

architecture rtl of acc is

-- All internal signals are defined here
  type state_type is (S0, S1, S2, S3, S4); -- States representing FSM states
  
  signal addr_ptr, next_addr_ptr: halfword_t;
  signal state, next_state: state_type;

begin

    -- Combinational logic for state transitions and output logic
    cl: process(reset, dataR, start, state, addr_ptr)
    begin
        next_state <= state;
        next_addr_ptr <= addr_ptr;
        en <= '0';
        we <= '0';
        finish <= '0';
        dataW <= (others => '0');
        addr <= (others => '0');
        
    case state is
        -- S0: Idle
        when S0 =>
            if start = '1' then
                next_state <= S1;
            end if;
        -- S1: Read
        when S1 =>
            en <= '1';
            we <= '0';
            addr <= addr_ptr;
            next_state <= S2;
        -- S2: Write
        when S2 =>
            en <= '1';
            we <= '1';
            addr <= std_logic_vector(unsigned(addr_ptr) + 25344); -- offset for the processed img address
            dataW <= not dataR;
            if unsigned(addr_ptr) = 25343 then -- if this was the last pixel
                next_state <= S3;
                next_addr_ptr <= (others => '0');
            else
                next_state <= S1;
                next_addr_ptr <= std_logic_vector(unsigned(addr_ptr) + 1);
            end if;
        -- S3: Finish
        when S3 =>
            finish <= '1';
            next_state <= S0;
        -- Default case
        when others =>
            next_state <= S0;
    end case;
            
    end process cl;

    myprocess : process(clk)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                state <= S0;
                addr_ptr <= (others => '0');
            else
                state <= next_state;
                addr_ptr <= next_addr_ptr;
            end if;
        end if;
    end process myprocess;

end rtl;

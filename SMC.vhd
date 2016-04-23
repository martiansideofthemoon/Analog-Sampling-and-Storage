library std;
library ieee;
use ieee.std_logic_1164.all;


package SMCComponents is
  component SMC is
    port(
      io: inout std_logic_vector(7 downto 0);
      mc_read_data: out std_logic_vector(7 downto 0);
      mc_write_data: in std_logic_vector(7 downto 0);
      mc_done: out std_logic;
      mc_start, mc_write: in std_logic;
      OE, WE, CS: out std_logic;
      mc_address: in std_logic_vector(12 downto 0);
      address: out std_logic_vector(12 downto 0);
      clk, reset: in std_logic
    );
  end component SMC;

  component ClockDivider2 is
  port (
    clk, reset: in std_logic;
    clk_slow: out std_logic
  );
  end component;

  component ClockDivider2Control is
  port (
    T0, T1: out std_logic;
    S: in std_logic;
    clk, reset: in std_logic;
    clk_slow: out std_logic
  );
  end component;

  component ClockDivider2Data is
  port (
    T0, T1: in std_logic;
    S: out std_logic;
    clk, reset: in std_logic
  );
  end component;

  --component DataRegister is
  --generic (data_width:integer);
  --port (Din: in std_logic_vector(data_width-1 downto 0);
  --      Dout: out std_logic_vector(data_width-1 downto 0);
  --      clk, enable: in std_logic);
  --end component DataRegister;

  --component Decrement9 is
  --      port (A: in std_logic_vector(8 downto 0); B: out std_logic_vector(8 downto 0));
  --end component Decrement9;

end package;

--library ieee;
--use ieee.std_logic_1164.all;
--entity DataRegister is
--  generic (data_width:integer);
--  port (Din: in std_logic_vector(data_width-1 downto 0);
--        Dout: out std_logic_vector(data_width-1 downto 0);
--        clk, enable: in std_logic);
--end entity;
--architecture Behave of DataRegister is
--begin
--    process(clk)
--    begin
--       if(clk'event and (clk  = '1')) then
--           if(enable = '1') then
--               Dout <= Din;
--           end if;
--       end if;
--    end process;
--end Behave;

--library ieee;
--use ieee.std_logic_1164.all;
--entity Decrement9 is
--   port (A: in std_logic_vector(8 downto 0); B: out std_logic_vector(8 downto 0));
--end entity Decrement9;
--architecture Serial of Decrement9 is
--begin
--  process(A)
--    variable borrow: std_logic;
--  begin
--    borrow := '1';
--    for I in 0 to 8 loop
--       B(I) <= A(I) xor borrow;
--       borrow := borrow and (not A(I));
--    end loop;
--  end process;
--end Serial;

library ieee;
use ieee.std_logic_1164.all;

library work;
use work.SMCComponents.all;
library work;
use work.ADCCComponents.all;
entity SMC is
    port(
      io: inout std_logic_vector(7 downto 0);
      mc_read_data: out std_logic_vector(7 downto 0);
      mc_write_data: in std_logic_vector(7 downto 0);
      mc_done: out std_logic;
      mc_start, mc_write: in std_logic;
      OE, WE, CS: out std_logic;
      mc_address: in std_logic_vector(12 downto 0);
      address: out std_logic_vector(12 downto 0);
      clk, reset: in std_logic
    );
end entity SMC;
architecture Struct of SMC is
    signal clk_slow: std_logic;
    type FsmState is (wait_state, set_OE_for_write, before_write, write_init, write_begin, writing, write_done,
                      set_CS_for_read, before_read, read_begin, read_done);
    signal state : FsmState;
    signal unclean_CS, unclean_WE, unclean_OE: std_logic;
    signal read_enable: std_logic := '0';
    signal address_enable: std_logic := '0';
begin
    process(clk_slow, reset, state)
      variable nstate: FsmState;
      variable nCS: std_logic := '1';
      variable nWE: std_logic := '1';
      variable nOE: std_logic := '1';
      variable read_enable_var: std_logic := '0';
      variable address_enable_var: std_logic := '0';
      variable nmc_done: std_logic;
      variable nmc_read_data: std_logic_vector(7 downto 0);
      variable nio: std_logic_vector(7 downto 0);
      variable naddress: std_logic_vector(12 downto 0);
    begin
      -- default values.
      nstate := state;
      read_enable_var := '0';
      address_enable_var := '0';
      nCS := '1';
      nWE := '1';
      nOE := '1';
      nmc_read_data := io;
      naddress := mc_address;
      nmc_done := '0';
      nio := "ZZZZZZZZ";
      case state is
        when wait_state =>
          if (mc_start = '1') then
            address_enable_var := '1';
            if (mc_write = '1') then
              nstate := set_OE_for_write;
            else
              nstate := set_CS_for_read;
            end if;
          end if;
        when set_OE_for_write =>
          nOE := '1';

          nstate := before_write;
        when before_write =>
          nCS := '0';
          nstate := write_init;
        when write_init =>
          nCS := '0';
          nWE := '0';
          nstate := write_begin;
        when write_begin =>
          nCS := '0';
          nWE := '0';
          nstate := writing;
        when writing =>
          nio := mc_write_data;
          nstate := write_done;
        when write_done =>
          nCS := '1';
          nWE := '1';
          nmc_done := '1';
          nstate := wait_state;
        when set_CS_for_read =>
          nCS := '0';
          nstate := before_read;
        when before_read =>
          nCS := '0';
          nOE := '0';
          nstate := read_begin;
        when read_begin =>
          nOE := '0';
          nCS := '0';
          --nmc_read_data := io;
          read_enable_var := '1';
          nstate := read_done;
        when read_done =>
          nmc_done := '1';
          nOE := '1';
          nCS := '1';
          nstate := wait_state;
      end case;
      if(reset = '1') then
        unclean_CS <= '1';
        unclean_WE <= '1';
        unclean_OE <= '1';
        mc_done <= '0';
        read_enable <= '0';
        address_enable <= '0';
        io <= "ZZZZZZZZ";
      else
        unclean_CS <= nCS;
        unclean_WE <= nWE;
        unclean_OE <= nOE;
        read_enable <= read_enable_var;
        address_enable <= address_enable_var;
        io <= nio;
        mc_done <= nmc_done;
       end if;
      if(clk_slow'event and clk_slow = '1') then
        if(reset = '1') then
          state <= wait_state;
        else
          state <= nstate;
        end if;
      end if;

    end process;

    CD: ClockDivider2
      port map(
        clk => clk,
        reset => reset,
        clk_slow => clk_slow
      );

    oe_clean: DataRegister
                   generic map (data_width => 1)
                   port map (Din(0) => unclean_OE,
                             Dout(0) => OE,
                             Enable => '1',
                             clk => clk_slow);
    we_clean: DataRegister
                   generic map (data_width => 1)
                   port map (Din(0) => unclean_WE,
                             Dout(0) => WE,
                             Enable => '1',
                             clk => clk_slow);
    cs_clean: DataRegister
                   generic map (data_width => 1)
                   port map (Din(0) => unclean_CS,
                             Dout(0) => CS,
                             Enable => '1',
                             clk => clk_slow);
    read_ram: DataRegister
                   generic map (data_width => 8)
                   port map (Din => io,
                             Dout => mc_read_data,
                             Enable => read_enable,
                             clk => clk_slow);
    address_ram: DataRegister
                   generic map (data_width => 13)
                   port map (Din => mc_address,
                             Dout => address,
                             Enable => address_enable,
                             clk => clk_slow);

end Struct;

library ieee;
use ieee.std_logic_1164.all;
library work;
use work.SMCComponents.all;
entity ClockDivider2 is
  port (
    clk, reset: in std_logic;
    clk_slow: out std_logic
  );
end entity ClockDivider2;

architecture Struct of ClockDivider2 is
   signal T0,T1,S: std_logic;
begin

    CP: ClockDivider2Control
      port map(
        T0 => T0,
        T1 => T1,
        S => S,
        clk_slow => clk_slow,
        reset => reset,
        clk => clk
      );

    DP: ClockDivider2Data
      port map (
        T0 => T0,
        T1 => T1,
        S => S,
        reset => reset,
        clk => clk
      );
end Struct;

library ieee;
use ieee.std_logic_1164.all;
entity ClockDivider2Control is
  port (
    T0, T1: out std_logic;
    S: in std_logic;
    clk, reset: in std_logic;
    clk_slow: out std_logic
  );
end entity;

architecture Behave of ClockDivider2Control is
   type FsmState is (rst, decrement);
   signal fsm_state : FsmState;
begin

   process(fsm_state, S, clk, reset)
      variable next_state: FsmState;
      variable Tvar: std_logic_vector(0 to 1);
      variable clk_slow_var: std_logic;
  begin
     -- defaults
    Tvar := (others => '0');
    next_state := fsm_state;
    clk_slow_var := '0';
    case fsm_state is
      when rst =>
          Tvar(0) := '1';
          next_state := decrement;
      when decrement =>
          Tvar(1) := '1';
          if (S = '1') then
            clk_slow_var := '1';
          else
            clk_slow_var := '0';
          end if;
    end case;

    T0 <= Tvar(0); T1 <= Tvar(1);
    clk_slow <= clk_slow_var;
    if(clk'event and (clk = '1')) then
      if(reset = '1') then
        fsm_state <= rst;
      else
        fsm_state <= next_state;
      end if;
    end if;
  end process;
end Behave;


library ieee;
use ieee.std_logic_1164.all;
library work;
use work.ADCCComponents.all;
library work;
use work.SMCComponents.all;
entity ClockDivider2Data is
  port (
    T0,T1: in std_logic;
    S: out std_logic;
    clk, reset: in std_logic
  );
end entity;

architecture Mixed of ClockDivider2Data is
    signal COUNT: std_logic_vector(8 downto 0);

    signal COUNT_in: std_logic_vector(8 downto 0);

    signal decrOut: std_logic_vector(8 downto 0);
    constant C_start : std_logic_vector(8 downto 0) := "011110111";

    signal count_enable: std_logic;

begin
    -- predicate
    S <= '1' when (COUNT(3) = '1') else '0';

    --------------------------------------------------------
    --  count-related logic
    --------------------------------------------------------
    -- decrementer
    decr: Decrement9  port map (A => COUNT, B => decrOut);

    -- count register.
    count_enable <=  (T0 or T1);
    COUNT_in <= decrOut when T1 = '1' else C_start;
    count_reg: DataRegister
                   generic map (data_width => 9)
                   port map (Din => COUNT_in,
                             Dout => COUNT,
                             Enable => count_enable,
                             clk => clk);
end Mixed;


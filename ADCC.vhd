library std;
library ieee;
use ieee.std_logic_1164.all;


package ADCCComponents is
  component ADCC is
    port(
      din: in std_logic_vector(7 downto 0);
      adc_data: out std_logic_vector(7 downto 0);
      CS, WR, RD: out std_logic;
      INTR: in std_logic;
      adc_run: in std_logic;
      adc_output_ready: out std_logic;
      clk, reset: in std_logic
    );
  end component ADCC;

  component ClockDivider is
  port (
    clk, reset: in std_logic;
    clk_slow: out std_logic
  );
  end component;

  component ClockDividerControl is
  port (
    T0, T1: out std_logic;
    S: in std_logic;
    clk, reset: in std_logic;
    clk_slow: out std_logic
  );
  end component;

  component ClockDividerData is
  port (
    T0, T1: in std_logic;
    S: out std_logic;
    clk, reset: in std_logic
  );
  end component;

  component DataRegister is
  generic (data_width:integer);
  port (Din: in std_logic_vector(data_width-1 downto 0);
        Dout: out std_logic_vector(data_width-1 downto 0);
        clk, enable: in std_logic);
  end component DataRegister;

  component Decrement9 is
        port (A: in std_logic_vector(8 downto 0); B: out std_logic_vector(8 downto 0));
  end component Decrement9;

end package;

library ieee;
use ieee.std_logic_1164.all;
entity DataRegister is
  generic (data_width:integer);
  port (Din: in std_logic_vector(data_width-1 downto 0);
        Dout: out std_logic_vector(data_width-1 downto 0);
        clk, enable: in std_logic);
end entity;
architecture Behave of DataRegister is
begin
    process(clk)
    begin
       if(clk'event and (clk  = '1')) then
           if(enable = '1') then
               Dout <= Din;
           end if;
       end if;
    end process;
end Behave;

library ieee;
use ieee.std_logic_1164.all;
entity Decrement9 is
   port (A: in std_logic_vector(8 downto 0); B: out std_logic_vector(8 downto 0));
end entity Decrement9;
architecture Serial of Decrement9 is
begin
  process(A)
    variable borrow: std_logic;
  begin
    borrow := '1';
    for I in 0 to 8 loop
       B(I) <= A(I) xor borrow;
       borrow := borrow and (not A(I));
    end loop;
  end process;
end Serial;

library ieee;
use ieee.std_logic_1164.all;

library work;
use work.ADCCComponents.all;
entity ADCC is
    port(din: in std_logic_vector(7 downto 0);
       adc_data: out std_logic_vector(7 downto 0);
       CS, WR, RD: out std_logic;
       INTR: in std_logic;
       adc_run: in std_logic;
       adc_output_ready: out std_logic;
       clk, reset: in std_logic
       );
end entity ADCC;
architecture Struct of ADCC is
    signal clk_slow: std_logic;
    type FsmState is (wait_state, adc_ready, begin_write, write_state, write_done,
      read_start, reading, done);
    signal state : FsmState;
begin
    process(clk_slow, reset, din, INTR, adc_run, state)
      variable nstate: FsmState;
      variable nCS, nWR, nRD, nop_ready: std_logic;
      variable nadc_data: std_logic_vector(7 downto 0);
    begin
      -- default values.
      nstate := state;
      nCS := '1';
      nWR := '1';
      nRD := '1';
      nop_ready := '0';
      nadc_data := din;
      case state is
        when wait_state =>
          if (adc_run = '1') then
            nCS := '0';
            nstate := adc_ready;
          end if;
        when adc_ready =>
          nCS := '0';
          nWR := '0';
          nstate := begin_write;
        when begin_write =>
          nCS := '0';
          nWR := '1';
          nstate := write_state;
        when write_state =>
          nCS := '0';
          if (INTR = '0') then
            nstate := write_done;
          end if;
        when write_done =>
          nCS := '0';
          nRD := '0';
          nstate := read_start;
        when read_start =>
          nCS := '0';
          nRD := '1';
          nstate := reading;
        when reading =>
          nCS := '0';
          nadc_data := din;
          nstate := done;
        when done =>
          nop_ready := '1';
          nCS := '1';
          nstate := wait_state;
      end case;
      if(reset = '1') then
        CS <= '1';
        RD <= '1';
        WR <= '1';
        adc_output_ready <= '0';
        adc_data <= "10101011";
      else
        CS <= nCS;
        RD <= nRD;
        WR <= nWR;
        adc_output_ready <= nop_ready;
        adc_data <= nadc_data;
       end if;

      if(clk_slow'event and clk_slow = '1') then
        if(reset = '1') then
          state <= wait_state;
        else
          state <= nstate;
        end if;
      end if;

    end process;

    CD: ClockDivider
      port map(
        clk => clk,
        reset => reset,
        clk_slow => clk_slow
      );

end Struct;

library ieee;
use ieee.std_logic_1164.all;
library work;
use work.ADCCComponents.all;
entity ClockDivider is
  port (
    clk, reset: in std_logic;
    clk_slow: out std_logic
  );
end entity ClockDivider;

architecture Struct of ClockDivider is
   signal T0,T1,S: std_logic;
begin

    CP: ClockDividerControl
      port map(
        T0 => T0,
        T1 => T1,
        S => S,
        clk_slow => clk_slow,
        reset => reset,
        clk => clk
      );

    DP: ClockDividerData
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
entity ClockDividerControl is
  port (
    T0, T1: out std_logic;
    S: in std_logic;
    clk, reset: in std_logic;
    clk_slow: out std_logic
  );
end entity;

architecture Behave of ClockDividerControl is
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

entity ClockDividerData is
  port (
    T0,T1: in std_logic;
    S: out std_logic;
    clk, reset: in std_logic
  );
end entity;

architecture Mixed of ClockDividerData is
    signal COUNT: std_logic_vector(8 downto 0);

    signal COUNT_in: std_logic_vector(8 downto 0);

    signal decrOut: std_logic_vector(8 downto 0);
    constant C_start : std_logic_vector(8 downto 0) := "011111111";

    signal count_enable: std_logic;

begin
    -- predicate
    S <= '1' when (COUNT(8) = '1') else '0';

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


library ieee;
use ieee.std_logic_1164.all;
library ieee;
use ieee.numeric_std.all;
library std;
use std.textio.all;

entity Testbench_TopLevel is
end entity;
architecture Behave of Testbench_TopLevel is
  component TopLevel is
  port (
    -- CCU outside world
    capture, display: in std_logic;
    dac_out: out std_logic_vector(7 downto 0);
    count_done: out std_logic;
    -- ADCC outside world
    din: in std_logic_vector(7 downto 0);
    INTR: in std_logic;
    CS, WR, RD: out std_logic;

    -- SMC outside world
    io: inout std_logic_vector(7 downto 0);
    OE, WE, CS_SMC: out std_logic;
    address: out std_logic_vector(12 downto 0);

    -- general
    clk, reset: in std_logic
  );
  end component;
  type mem_type is array(8191 downto 0) of std_logic_vector(7 downto 0);
  signal capture, display, count_done: std_logic;
  signal dac_out: std_logic_vector(7 downto 0);
  signal din: std_logic_vector(7 downto 0);
  signal INTR: std_logic;
  signal CS,WR,RD: std_logic;
  signal io: std_logic_vector(7 downto 0);
  signal OE, WE, CS_SMC: std_logic;
  signal address: std_logic_vector(12 downto 0);
  signal clk: std_logic := '0';
  signal reset: std_logic := '1';

  function to_string(x: string) return string is
      variable ret_val: string(1 to x'length);
      alias lx : string (1 to x'length) is x;
  begin
      ret_val := lx;
      return(ret_val);
  end to_string;

  function to_std_logic_vector(x: bit_vector) return std_logic_vector is
    alias lx: bit_vector(1 to x'length) is x;
    variable ret_var : std_logic_vector(1 to x'length);
  begin
     for I in 1 to x'length loop
        if(lx(I) = '1') then
           ret_var(I) :=  '1';
        else
           ret_var(I) :=  '0';
	end if;
     end loop;
     return(ret_var);
  end to_std_logic_vector;

  function vec2int(vec1: bit_vector)
    return integer is
  variable retval: integer:=0;
  alias vec: bit_vector(vec1'length-1 downto 0) is vec1;
  begin
    for i in vec'high downto 1 loop
      if (vec(i)='1') then
        retval:=(retval+1)*2;
      else
        retval:=retval*2;
      end if;
    end loop;
    if vec(0)='1' then
      retval:=retval+1;
    end if;
    return retval;
  end vec2int;

begin
  clk <= not clk after 10 ns; -- assume 10ns clock.

  process
    begin
      wait until clk = '1';
      reset <= '0';
      wait;
  end process;

  process
    variable err_flag : boolean := false;
    File INFILE: text open read_mode is "TRACEFILE_TopLevel.txt";
    FILE OUTFILE: text  open write_mode is "OUTPUTS_TopLevel.txt";

    ---------------------------------------------------
    -- edit the next few lines to customize
    variable mode: bit_vector (0 downto 0); -- capture / display
    variable data: bit_vector (7 downto 0); -- analog signal
    variable addr: bit_vector (13 downto 0);
    variable ram: mem_type;
    ----------------------------------------------------
    variable INPUT_LINE: Line;
    variable OUTPUT_LINE: Line;
    variable LINE_COUNT: integer := 0;
    begin

    wait until clk = '1';

    while not endfile(INFILE) loop
    	  wait until clk = '0';

        LINE_COUNT := LINE_COUNT + 1;

	  readLine (INFILE, INPUT_LINE);
          read (INPUT_LINE, mode);
          read (INPUT_LINE, addr);
          read (INPUT_LINE, data);

    if (to_std_logic_vector(mode) = "1") then
      ram(vec2int(addr)) := to_std_logic_vector(data);
    end if;

      if (mode = "1") then
      -- write routine
        INTR <= '1';
        capture <= '1';
        display <= '0';
        -- wait until WR = '0' for ADC
        while (true) loop
          wait until clk = '1';
          if (WR = '0') then
            din <= to_std_logic_vector(data);
            INTR <= '0' after 215 us;
            exit;
          end if;
        end loop;
        -- wait until WE is '0' for MC
        while (true) loop
          wait until clk = '1';
          if (WE = '0') then
            INTR <= '1';
            io <= "ZZZZZZZZ";
            exit;
          end if;
        end loop;
        capture <= '0';
        -- wait until count_done = '1'
        while (true) loop
          wait until clk = '1';
          if (count_done = '1') then
            exit;
          end if;
        end loop;
      else
      -- read routine
        capture <= '0';
        display <= '1';
        -- wait until OE is '0'
        while (true) loop
        wait until clk = '1';
          if (OE = '0') then
            io <= ram(vec2int(addr)) after 35 ns;
            exit;
          end if;
        end loop;
        -- wait until OE is '1'
        while (true) loop
        wait until clk = '1';
          if (OE = '1') then
            io <= "ZZZZZZZZ" after 35 ns;
            exit;
          end if;
        end loop;
        display <= '0';
        -- wait until count_done = '1'
        while (true) loop
          wait until clk = '1';
          if (count_done = '1') then
            exit;
          end if;
        end loop;
        wait for 20 ns;
      end if;
          --------------------------------------
	  -- check outputs.
	  if (mode = "0" and dac_out /= to_std_logic_vector(data)) then
      write(OUTPUT_LINE,to_string("ERROR: in RESULT, line "));
      write(OUTPUT_LINE, LINE_COUNT);
      writeline(OUTFILE, OUTPUT_LINE);
      err_flag := true;
    end if;
          --------------------------------------
    end loop;

    assert (err_flag) report "SUCCESS, all tests passed." severity note;
    assert (not err_flag) report "FAILURE, some tests failed." severity error;

    wait;
  end process;

  dut: TopLevel
  port map(
    -- CCU outside world
    capture => capture,
    display => display,
    dac_out => dac_out,
    count_done => count_done,
    -- ADCC outside world
    din => din,
    INTR => INTR,
    CS => CS, WR => WR, RD => RD,
    -- SMC outside world
    io => io,
    OE => OE, WE => WE, CS_SMC => CS_SMC,
    address => address,
    -- general
    clk => clk, reset => reset
  );

end Behave;


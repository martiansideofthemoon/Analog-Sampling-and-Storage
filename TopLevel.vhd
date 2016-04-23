library std;
library ieee;
use ieee.std_logic_1164.all;
library work;
use work.ADCCComponents.all;
library work;
use work.SMCComponents.all;
library work;
use work.CCUComponents.all;

entity TopLevel is
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
end entity TopLevel;

architecture Struct of TopLevel is
	-- ADCC signals
	signal adc_run: std_logic;
	signal adc_data: std_logic_vector(7 downto 0);
	signal adc_output_ready: std_logic;
	-- SMC signals
	signal mc_start, mc_write, mc_done: std_logic;
	signal mc_read_data: std_logic_vector(7 downto 0);
	signal mc_write_data: std_logic_vector(7 downto 0);
	signal mc_address: std_logic_vector(12 downto 0);

begin
	adc_control: ADCC
    port map(
      din => din,
      adc_data => adc_data,
      CS => CS, WR => WR, RD => RD,
      INTR => INTR,
      adc_run => adc_run,
      adc_output_ready => adc_output_ready,
      clk => clk, reset => reset
    );

    central_control: CCU
    port map(
      adc_run => adc_run,
      adc_output_ready => adc_output_ready,
      mc_start => mc_start,
      mc_write => mc_write,
      mc_done => mc_done,
      capture => capture,
      display => display,
      mc_read_data => mc_read_data,
      mc_write_data => mc_write_data,
      adc_data => adc_data,
      mc_address => mc_address,
      dac_out => dac_out,
      count_done_out => count_done,
      clk => clk,
      reset => reset
    );

    memory_control: SMC
    port map(
      io => io,
      mc_read_data => mc_read_data,
      mc_write_data => mc_write_data,
      mc_done => mc_done,
      mc_start => mc_start,
      mc_write => mc_write,
      OE => OE,
      WE => WE,
      CS => CS_SMC,
      mc_address => mc_address,
      address => address,
      clk => clk,
      reset => reset
    );

end Struct;
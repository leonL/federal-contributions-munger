require './lib/represent_postal_code_concordance.rb'
require 'csv'; CSV::Converters[:na_to_nil] = Proc.new {|val| val == "NA" ? nil : val}

class ConcordanceWriter

  def self.run
    concord_writer = new
    concord_writer.fetch_data_and_write_csv
  end

  def initialize
    @concordance = CSV.open(
      'data/output/postal_code_geo_concordance.csv', 'a', {quote_char: '"', force_quotes: true}
    )

    @invalid_codes = CSV.open(
      'data/output/invalid_postal_codes.csv', 'a', {quote_char: '"', force_quotes: true}
    )
  end

  def fetch_data_and_write_csv
    csv_params = {headers: true, header_converters: :symbol,
      converters: :na_to_nil}

    CSV.foreach(
      'data/src/postal_codes.csv', csv_params) do |record|

      concordance = fetch_data_for_postal_code(record[:postal_code])
      if concordance.not_found?
        puts "It seems that #{record[:postal_code]} is NOT a valid postal code."
        @invalid_codes << [record[:postal_code]]
      else
        concordance.ridings.each do |riding|
          row = complete_record(record, riding, concordance)
          puts "Inserting... #{row.inspect}"
          @concordance << row
        end
      end
    end
    @invalid_codes.close; @concordance.close
  end

private

  def fetch_data_for_postal_code(pcode)
    sleep(1) # stay within represent.opennorth.ca/api rate limit
    ::RepresentPostalCodeConcordance.new(pcode)
  end

  def complete_record(incomplete_record, riding, concord)
    lat = incomplete_record[:latitude] || concord.latitude
    long = incomplete_record[:longitude] || concord.longitude
    city = incomplete_record[:city] || concord.city
    province = incomplete_record[:province] || concord.province

    [incomplete_record[:postal_code], riding[:id], riding[:name], lat, long, city, province]
  end
end

ConcordanceWriter.run
require 'httparty'
require 'titleize'

class RepresentPostalCodeConcordance

  attr_accessor :response

  def initialize(postal_code)
    self.response = find_by_postal_code(postal_code)
  end

  def find_by_postal_code(postal_code)
    HTTParty.get("http://represent.opennorth.ca/postcodes/#{postal_code}",
        query: {sets: "federal-electoral-districts"})
  end

  def not_found?
    response.response.code == "404"
  end

  def ridings
    @ridings ||= begin
      response_ridings = response["boundaries_centroid"] | response["boundaries_concordance"]
      ridings = []; response_ridings.each do |riding|
        ridings << {name: riding["name"], id: riding["external_id"]}
      end
      ridings
    end
  end

  def province
    format_province_str(response["province"])
  end

  def city
    format_city_str(response["city"])
  end

  def latitude
    response["centroid"]["coordinates"][0]
  end

  def longitude
    response["centroid"]["coordinates"][1]
  end

private

  def format_city_str(str)
    str.strip.downcase.titleize
  end

  def format_province_str(str)
    str.strip.upcase
  end
end
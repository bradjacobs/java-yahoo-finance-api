/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.types;

import com.github.bradjacobs.yahoofinance.types.criteria.CriteriaEnum;

import javax.annotation.Generated;

import static com.github.bradjacobs.yahoofinance.types.Sector.*;

@Generated(value="yahoo-finance-api-internal-tools", date="2021-07-06")
public enum Industry implements CriteriaEnum
{
    
    // Basic Materials
    AGRICULTURAL_INPUTS("Agricultural Inputs", BASIC_MATERIALS),
    ALUMINUM("Aluminum", BASIC_MATERIALS),
    BUILDING_MATERIALS("Building Materials", BASIC_MATERIALS),
    CHEMICALS("Chemicals", BASIC_MATERIALS),
    COKING_COAL("Coking Coal", BASIC_MATERIALS),
    COPPER("Copper", BASIC_MATERIALS),
    GOLD("Gold", BASIC_MATERIALS),
    LUMBER_AND_WOOD_PRODUCTION("Lumber & Wood Production", BASIC_MATERIALS),
    OTHER_INDUSTRIAL_METALS_AND_MINING("Other Industrial Metals & Mining", BASIC_MATERIALS),
    OTHER_PRECIOUS_METALS_AND_MINING("Other Precious Metals & Mining", BASIC_MATERIALS),
    PAPER_AND_PAPER_PRODUCTS("Paper & Paper Products", BASIC_MATERIALS),
    SILVER("Silver", BASIC_MATERIALS),
    SPECIALTY_CHEMICALS("Specialty Chemicals", BASIC_MATERIALS),
    STEEL("Steel", BASIC_MATERIALS),
    
    // Communication Services
    ADVERTISING_AGENCIES("Advertising Agencies", COMMUNICATION_SERVICES),
    BROADCASTING("Broadcasting", COMMUNICATION_SERVICES),
    ELECTRONIC_GAMING_AND_MULTIMEDIA("Electronic Gaming & Multimedia", COMMUNICATION_SERVICES),
    ENTERTAINMENT("Entertainment", COMMUNICATION_SERVICES),
    INTERNET_CONTENT_AND_INFORMATION("Internet Content & Information", COMMUNICATION_SERVICES),
    PUBLISHING("Publishing", COMMUNICATION_SERVICES),
    TELECOM_SERVICES("Telecom Services", COMMUNICATION_SERVICES),
    
    // Consumer Cyclical
    APPAREL_MANUFACTURING("Apparel Manufacturing", CONSUMER_CYCLICAL),
    APPAREL_RETAIL("Apparel Retail", CONSUMER_CYCLICAL),
    AUTO_AND_TRUCK_DEALERSHIPS("Auto & Truck Dealerships", CONSUMER_CYCLICAL),
    AUTO_MANUFACTURERS("Auto Manufacturers", CONSUMER_CYCLICAL),
    AUTO_PARTS("Auto Parts", CONSUMER_CYCLICAL),
    DEPARTMENT_STORES("Department Stores", CONSUMER_CYCLICAL),
    FOOTWEAR_AND_ACCESSORIES("Footwear & Accessories", CONSUMER_CYCLICAL),
    FURNISHINGS_FIXTURES_AND_APPLIANCES("Furnishings, Fixtures & Appliances", CONSUMER_CYCLICAL),
    GAMBLING("Gambling", CONSUMER_CYCLICAL),
    HOME_IMPROVEMENT_RETAIL("Home Improvement Retail", CONSUMER_CYCLICAL),
    INTERNET_RETAIL("Internet Retail", CONSUMER_CYCLICAL),
    LEISURE("Leisure", CONSUMER_CYCLICAL),
    LODGING("Lodging", CONSUMER_CYCLICAL),
    LUXURY_GOODS("Luxury Goods", CONSUMER_CYCLICAL),
    PACKAGING_AND_CONTAINERS("Packaging & Containers", CONSUMER_CYCLICAL),
    PERSONAL_SERVICES("Personal Services", CONSUMER_CYCLICAL),
    RECREATIONAL_VEHICLES("Recreational Vehicles", CONSUMER_CYCLICAL),
    RESIDENTIAL_CONSTRUCTION("Residential Construction", CONSUMER_CYCLICAL),
    RESORTS_AND_CASINOS("Resorts & Casinos", CONSUMER_CYCLICAL),
    RESTAURANTS("Restaurants", CONSUMER_CYCLICAL),
    SPECIALTY_RETAIL("Specialty Retail", CONSUMER_CYCLICAL),
    TEXTILE_MANUFACTURING("Textile Manufacturing", CONSUMER_CYCLICAL),
    TRAVEL_SERVICES("Travel Services", CONSUMER_CYCLICAL),
    
    // Consumer Defensive
    BEVERAGES_BREWERS("Beverages—Brewers", CONSUMER_DEFENSIVE),
    BEVERAGES_NON_ALCOHOLIC("Beverages—Non-Alcoholic", CONSUMER_DEFENSIVE),
    BEVERAGES_WINERIES_AND_DISTILLERIES("Beverages—Wineries & Distilleries", CONSUMER_DEFENSIVE),
    CONFECTIONERS("Confectioners", CONSUMER_DEFENSIVE),
    DISCOUNT_STORES("Discount Stores", CONSUMER_DEFENSIVE),
    EDUCATION_AND_TRAINING_SERVICES("Education & Training Services", CONSUMER_DEFENSIVE),
    FARM_PRODUCTS("Farm Products", CONSUMER_DEFENSIVE),
    FOOD_DISTRIBUTION("Food Distribution", CONSUMER_DEFENSIVE),
    GROCERY_STORES("Grocery Stores", CONSUMER_DEFENSIVE),
    HOUSEHOLD_AND_PERSONAL_PRODUCTS("Household & Personal Products", CONSUMER_DEFENSIVE),
    PACKAGED_FOODS("Packaged Foods", CONSUMER_DEFENSIVE),
    TOBACCO("Tobacco", CONSUMER_DEFENSIVE),
    
    // Energy
    OIL_AND_GAS_DRILLING("Oil & Gas Drilling", ENERGY),
    OIL_AND_GAS_EQUIPMENT_AND_SERVICES("Oil & Gas Equipment & Services", ENERGY),
    OIL_AND_GAS_E_AND_P("Oil & Gas E&P", ENERGY),
    OIL_AND_GAS_INTEGRATED("Oil & Gas Integrated", ENERGY),
    OIL_AND_GAS_MIDSTREAM("Oil & Gas Midstream", ENERGY),
    OIL_AND_GAS_REFINING_AND_MARKETING("Oil & Gas Refining & Marketing", ENERGY),
    THERMAL_COAL("Thermal Coal", ENERGY),
    URANIUM("Uranium", ENERGY),
    
    // Financial Services
    ASSET_MANAGEMENT("Asset Management", FINANCIAL_SERVICES),
    BANKS_DIVERSIFIED("Banks—Diversified", FINANCIAL_SERVICES),
    BANKS_REGIONAL("Banks—Regional", FINANCIAL_SERVICES),
    CAPITAL_MARKETS("Capital Markets", FINANCIAL_SERVICES),
    CREDIT_SERVICES("Credit Services", FINANCIAL_SERVICES),
    FINANCIAL_CONGLOMERATES("Financial Conglomerates", FINANCIAL_SERVICES),
    FINANCIAL_DATA_AND_STOCK_EXCHANGES("Financial Data & Stock Exchanges", FINANCIAL_SERVICES),
    INSURANCE_BROKERS("Insurance Brokers", FINANCIAL_SERVICES),
    INSURANCE_DIVERSIFIED("Insurance—Diversified", FINANCIAL_SERVICES),
    INSURANCE_LIFE("Insurance—Life", FINANCIAL_SERVICES),
    INSURANCE_PROPERTY_AND_CASUALTY("Insurance—Property & Casualty", FINANCIAL_SERVICES),
    INSURANCE_REINSURANCE("Insurance—Reinsurance", FINANCIAL_SERVICES),
    INSURANCE_SPECIALTY("Insurance—Specialty", FINANCIAL_SERVICES),
    MORTGAGE_FINANCE("Mortgage Finance", FINANCIAL_SERVICES),
    SHELL_COMPANIES("Shell Companies", FINANCIAL_SERVICES),
    
    // Healthcare
    BIOTECHNOLOGY("Biotechnology", HEALTHCARE),
    DIAGNOSTICS_AND_RESEARCH("Diagnostics & Research", HEALTHCARE),
    DRUG_MANUFACTURERS_GENERAL("Drug Manufacturers—General", HEALTHCARE),
    DRUG_MANUFACTURERS_SPECIALTY_AND_GENERIC("Drug Manufacturers—Specialty & Generic", HEALTHCARE),
    HEALTHCARE_PLANS("Healthcare Plans", HEALTHCARE),
    HEALTH_INFORMATION_SERVICES("Health Information Services", HEALTHCARE),
    MEDICAL_CARE_FACILITIES("Medical Care Facilities", HEALTHCARE),
    MEDICAL_DEVICES("Medical Devices", HEALTHCARE),
    MEDICAL_DISTRIBUTION("Medical Distribution", HEALTHCARE),
    MEDICAL_INSTRUMENTS_AND_SUPPLIES("Medical Instruments & Supplies", HEALTHCARE),
    PHARMACEUTICAL_RETAILERS("Pharmaceutical Retailers", HEALTHCARE),
    
    // Industrials
    AEROSPACE_AND_DEFENSE("Aerospace & Defense", INDUSTRIALS),
    AIRLINES("Airlines", INDUSTRIALS),
    AIRPORTS_AND_AIR_SERVICES("Airports & Air Services", INDUSTRIALS),
    BUILDING_PRODUCTS_AND_EQUIPMENT("Building Products & Equipment", INDUSTRIALS),
    BUSINESS_EQUIPMENT_AND_SUPPLIES("Business Equipment & Supplies", INDUSTRIALS),
    CONGLOMERATES("Conglomerates", INDUSTRIALS),
    CONSULTING_SERVICES("Consulting Services", INDUSTRIALS),
    ELECTRICAL_EQUIPMENT_AND_PARTS("Electrical Equipment & Parts", INDUSTRIALS),
    ENGINEERING_AND_CONSTRUCTION("Engineering & Construction", INDUSTRIALS),
    FARM_AND_HEAVY_CONSTRUCTION_MACHINERY("Farm & Heavy Construction Machinery", INDUSTRIALS),
    INDUSTRIAL_DISTRIBUTION("Industrial Distribution", INDUSTRIALS),
    INFRASTRUCTURE_OPERATIONS("Infrastructure Operations", INDUSTRIALS),
    INTEGRATED_FREIGHT_AND_LOGISTICS("Integrated Freight & Logistics", INDUSTRIALS),
    MARINE_SHIPPING("Marine Shipping", INDUSTRIALS),
    METAL_FABRICATION("Metal Fabrication", INDUSTRIALS),
    POLLUTION_AND_TREATMENT_CONTROLS("Pollution & Treatment Controls", INDUSTRIALS),
    RAILROADS("Railroads", INDUSTRIALS),
    RENTAL_AND_LEASING_SERVICES("Rental & Leasing Services", INDUSTRIALS),
    SECURITY_AND_PROTECTION_SERVICES("Security & Protection Services", INDUSTRIALS),
    SPECIALTY_BUSINESS_SERVICES("Specialty Business Services", INDUSTRIALS),
    SPECIALTY_INDUSTRIAL_MACHINERY("Specialty Industrial Machinery", INDUSTRIALS),
    STAFFING_AND_EMPLOYMENT_SERVICES("Staffing & Employment Services", INDUSTRIALS),
    TOOLS_AND_ACCESSORIES("Tools & Accessories", INDUSTRIALS),
    TRUCKING("Trucking", INDUSTRIALS),
    WASTE_MANAGEMENT("Waste Management", INDUSTRIALS),
    
    // Real Estate
    REAL_ESTATE_DEVELOPMENT("Real Estate—Development", REAL_ESTATE),
    REAL_ESTATE_DIVERSIFIED("Real Estate—Diversified", REAL_ESTATE),
    REAL_ESTATE_SERVICES("Real Estate Services", REAL_ESTATE),
    REIT_DIVERSIFIED("REIT—Diversified", REAL_ESTATE),
    REIT_HEALTHCARE_FACILITIES("REIT—Healthcare Facilities", REAL_ESTATE),
    REIT_HOTEL_AND_MOTEL("REIT—Hotel & Motel", REAL_ESTATE),
    REIT_INDUSTRIAL("REIT—Industrial", REAL_ESTATE),
    REIT_MORTGAGE("REIT—Mortgage", REAL_ESTATE),
    REIT_OFFICE("REIT—Office", REAL_ESTATE),
    REIT_RESIDENTIAL("REIT—Residential", REAL_ESTATE),
    REIT_RETAIL("REIT—Retail", REAL_ESTATE),
    REIT_SPECIALTY("REIT—Specialty", REAL_ESTATE),
    
    // Technology
    COMMUNICATION_EQUIPMENT("Communication Equipment", TECHNOLOGY),
    COMPUTER_HARDWARE("Computer Hardware", TECHNOLOGY),
    CONSUMER_ELECTRONICS("Consumer Electronics", TECHNOLOGY),
    ELECTRONICS_AND_COMPUTER_DISTRIBUTION("Electronics & Computer Distribution", TECHNOLOGY),
    ELECTRONIC_COMPONENTS("Electronic Components", TECHNOLOGY),
    INFORMATION_TECHNOLOGY_SERVICES("Information Technology Services", TECHNOLOGY),
    SCIENTIFIC_AND_TECHNICAL_INSTRUMENTS("Scientific & Technical Instruments", TECHNOLOGY),
    SEMICONDUCTORS("Semiconductors", TECHNOLOGY),
    SEMICONDUCTOR_EQUIPMENT_AND_MATERIALS("Semiconductor Equipment & Materials", TECHNOLOGY),
    SOFTWARE_APPLICATION("Software—Application", TECHNOLOGY),
    SOFTWARE_INFRASTRUCTURE("Software—Infrastructure", TECHNOLOGY),
    SOLAR("Solar", TECHNOLOGY),
    
    // Utilities
    UTILITIES_DIVERSIFIED("Utilities—Diversified", UTILITIES),
    UTILITIES_INDEPENDENT_POWER_PRODUCERS("Utilities—Independent Power Producers", UTILITIES),
    UTILITIES_REGULATED_ELECTRIC("Utilities—Regulated Electric", UTILITIES),
    UTILITIES_REGULATED_GAS("Utilities—Regulated Gas", UTILITIES),
    UTILITIES_REGULATED_WATER("Utilities—Regulated Water", UTILITIES),
    UTILITIES_RENEWABLE("Utilities—Renewable", UTILITIES);



    private final String value;
    private final Sector sector;

    Industry(String value, Sector sector) {
        this.value = value;
        this.sector = sector;
    }

    public Sector getSector() {
        return sector;
    }

    public String getValue() {
        return value;
    }

    @Override
    public String getCriteriaValue() {
        return value;
    }
}

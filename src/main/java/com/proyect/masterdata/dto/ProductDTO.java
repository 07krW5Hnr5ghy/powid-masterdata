package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.OffsetDateTime;
import java.util.Date;
import java.util.List;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ProductDTO {
    private String sku;
    private String brand;
    private String model;
    private String size;
    private String category;
    private String subCategory;
    private String color;
    private String unit;
    private Double price;
    private List<String> pictures;
    private Boolean pictureFlag;
    private OffsetDateTime registrationDate;
    private OffsetDateTime updateDate;
}

package com.proyect.masterdata.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestProductExcel {
    private String sku;
    private String model;
    private String color;
    private String category;
    private String size;
    private String unit;
    private Double price;
}

package com.proyect.masterdata.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestProductSave {
    private String sku;
    private String model;
    private String color;
    private String category;
    private String size;
    private String unit;
    private Double price;
    private String characteristics;
}

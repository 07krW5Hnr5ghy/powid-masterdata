package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;
import java.util.List;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class OrderItemDTO {
    private Long orderId;
    private String sku;
    private String color;
    private String size;
    private String category;
    private String unit;
    private Integer quantity;
    private Double unitPrice;
    private String discount;
    private Double discountAmount;
    private Double totalPrice;
    private String observations;
    private List<String> pictures;
    private Date registrationDate;
    private Date updateDate;
}

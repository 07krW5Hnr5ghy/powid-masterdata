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
public class OrderDTO {
    private Long id;
    private String sellerName;
    private String customerName;
    private String customerType;
    private String customerPhone;
    private String instagram;
    private String department;
    private String province;
    private String district;
    private String address;
    private String managementType;
    private String paymentType;
    private String saleChannel;
    private String reference;
    private String paymentReceipt;
    private Double saleAmount;
    private Double deliveryAmount;
    private Double advancedPayment;
    private Double duePayment;
    private Date registrationDate;
    private Date updateDate;
    private String deliveryAddress;
    private String deliveryMan;
    private String deliveryPhone;
    private List<ItemDTO> items;
    private String orderStatus;
}

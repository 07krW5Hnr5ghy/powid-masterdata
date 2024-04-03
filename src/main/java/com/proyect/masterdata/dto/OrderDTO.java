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
    private Long serial;
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
    private String paymentMethod;
    private String saleChannel;
    private String reference;
    private List<String> paymentReceipts;
    private List<String> courierPictures;
    private Double saleAmount;
    private Double deliveryAmount;
    private Double advancedPayment;
    private Double duePayment;
    private Date registrationDate;
    private Date updateDate;
    private String deliveryAddress;
    private String courier;
    private List<OrderItemDTO> items;
    private String orderStatus;
}

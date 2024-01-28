package com.proyect.masterdata.dto.request;

import com.proyect.masterdata.domain.Item;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestOrderSave {

    private String seller;

    private String observations;

    private String paymentReceipt;

    private String deliveryAddress;

    private Double deliveryAmount;

    private Double advancedPayment;

    private String saleChannel;

    private String paymentMethod;

    private String managementType;

    private List<RequestItem> requestItems;

    private String customerName;

    private String customerType;

    private String instagram;

    private String customerPhone;

    private String customerAddress;

    private String customerDistrict;

    private String customerProvince;

    private String customerDepartment;

    private String customerReference;

}

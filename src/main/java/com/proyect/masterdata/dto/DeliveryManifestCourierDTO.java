package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.OffsetDateTime;
import java.util.UUID;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class DeliveryManifestCourierDTO {
    private UUID deliveryManifestId;
    private UUID courierId;
    //private UUID stockTransactionId;
    private Long manifestNumber;
    private String warehouse;
    private Boolean open;
    private Boolean isExists;
    private Integer paid;
    private Integer quantityOrders;
    private Integer receivable;
    private Integer delivered;
    private String observations;
    private OffsetDateTime registrationDate;
    private OffsetDateTime updateDate;
}

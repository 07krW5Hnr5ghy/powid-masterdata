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
public class DeliveryPointDTO {
    private String name;
    private String user;
    private UUID id;
    private String address;
    private OffsetDateTime registrationDate;
    private OffsetDateTime updateDate;
    private Boolean status;
}

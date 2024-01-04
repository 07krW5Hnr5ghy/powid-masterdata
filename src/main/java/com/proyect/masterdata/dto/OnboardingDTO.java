package com.proyect.masterdata.dto;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class OnboardingDTO {

    private String username;

    private String name;

    private String surname;

    private String email;

    private String address;

    private String mobile;

    private String dni;

    private String category;

    private Integer usersMinimum;

    private Integer usersMaximum;

    private Boolean billing;

    private String comment;

    private String businessName;

    private String businessRuc;

    private String gender;

    private String district;

    private String store;

    private String storeUrl;

    private String storeType;

    private List<String> closingChannels;

    private List<String> modules;

    private String entryChannel;

    private Boolean demo;
}

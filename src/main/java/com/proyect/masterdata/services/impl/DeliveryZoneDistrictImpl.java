package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.DeliveryZoneDistrictDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IDeliveryZoneDistrict;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class DeliveryZoneDistrictImpl implements IDeliveryZoneDistrict {
    private final UserRepository userRepository;
    private final DeliveryZoneRepository deliveryZoneRepository;
    private final DistrictRepository districtRepository;
    private final DeliveryZoneDistrictRepository deliveryZoneDistrictRepository;
    private final IAudit iAudit;
    private final ProvinceRepository provinceRepository;
    private final DeliveryZoneDistrictRepositoryCustom deliveryZoneDistrictRepositoryCustom;
    @Override
    public ResponseSuccess save(String deliveryZoneName, String districtName, String provinceName, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        DeliveryZone deliveryZone;
        District district;
        DeliveryZoneDistrict deliveryZoneDistrict;
        Province province;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user==null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }else{
            deliveryZone = deliveryZoneRepository.findByNameAndClientId(deliveryZoneName.toUpperCase(),user.getClientId());
            province = provinceRepository.findByNameAndStatusTrue(provinceName.toUpperCase());
        }

        if (deliveryZone == null) {
            throw new BadRequestExceptions(Constants.ErrorDeliveryZone);
        }

        if (province == null) {
            throw new BadRequestExceptions(Constants.ErrorProvince);
        }else{
            district = districtRepository.findByNameAndProvinceIdAndStatusTrue(districtName.toUpperCase(),province.getId());
        }

        if(district==null){
            throw new BadRequestExceptions(Constants.ErrorDistrict);
        }else{
            deliveryZoneDistrict = deliveryZoneDistrictRepository.findByDeliveryZoneIdAndDistrictId(deliveryZone.getId(),district.getId());
        }

        if(deliveryZoneDistrict!=null){
            throw new BadRequestExceptions(Constants.ErrorDeliveryZoneDistrictExists);
        }

        try {
            DeliveryZoneDistrict newDeliveryZoneDistrict = deliveryZoneDistrictRepository.save(DeliveryZoneDistrict.builder()
                            .deliveryZoneId(deliveryZone.getId())
                            .deliveryZone(deliveryZone)
                            .district(district)
                            .districtId(district.getId())
                            .registrationDate(OffsetDateTime.now())
                            .updateDate(OffsetDateTime.now())
                            .status(true)
                            .user(user)
                            .userId(user.getId())
                            .client(user.getClient())
                            .clientId(user.getClientId())
                    .build());
            iAudit.save("ADD_DELIVERY_ZONE_DISTRICT",
                    "DISTRITO "+
                            newDeliveryZoneDistrict.getDistrict().getName()+
                            " AGREGADO A ZONA DE DELIVERY "+
                            newDeliveryZoneDistrict.getDeliveryZone().getName()+".",
                    newDeliveryZoneDistrict.getUser().getUsername(),
                    user.getUsername());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<ResponseSuccess> saveAsync(String deliveryZoneName, String districtName, String provinceName, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            DeliveryZone deliveryZone;
            District district;
            DeliveryZoneDistrict deliveryZoneDistrict;
            Province province;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user==null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                deliveryZone = deliveryZoneRepository.findByNameAndClientId(deliveryZoneName.toUpperCase(),user.getClientId());
                province = provinceRepository.findByNameAndStatusTrue(provinceName.toUpperCase());
            }

            if (deliveryZone == null) {
                throw new BadRequestExceptions(Constants.ErrorDeliveryZone);
            }

            if (province == null) {
                throw new BadRequestExceptions(Constants.ErrorProvince);
            }else{
                district = districtRepository.findByNameAndProvinceIdAndStatusTrue(districtName.toUpperCase(),province.getId());
            }

            if(district==null){
                throw new BadRequestExceptions(Constants.ErrorDistrict);
            }else{
                deliveryZoneDistrict = deliveryZoneDistrictRepository.findByDeliveryZoneIdAndDistrictId(deliveryZone.getId(),district.getId());
            }

            if(deliveryZoneDistrict!=null){
                throw new BadRequestExceptions(Constants.ErrorDeliveryZoneDistrictExists);
            }

            try {
                DeliveryZoneDistrict newDeliveryZoneDistrict = deliveryZoneDistrictRepository.save(DeliveryZoneDistrict.builder()
                        .deliveryZoneId(deliveryZone.getId())
                        .deliveryZone(deliveryZone)
                        .district(district)
                        .districtId(district.getId())
                        .registrationDate(OffsetDateTime.now())
                        .updateDate(OffsetDateTime.now())
                        .status(true)
                        .user(user)
                        .userId(user.getId())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .build());
                iAudit.save("ADD_DELIVERY_ZONE_DISTRICT",
                        "DISTRITO "+
                                newDeliveryZoneDistrict.getDistrict().getName()+
                                " AGREGADO A ZONA DE DELIVERY "+
                                newDeliveryZoneDistrict.getDeliveryZone().getName()+".",
                        newDeliveryZoneDistrict.getUser().getUsername(),
                        user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(String deliveryZoneName, String districtName,String provinceName, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            DeliveryZone deliveryZone;
            District district;
            DeliveryZoneDistrict deliveryZoneDistrict;
            Province province;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user==null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                deliveryZone = deliveryZoneRepository.findByNameAndClientId(deliveryZoneName.toUpperCase(),user.getClientId());
                province = provinceRepository.findByNameAndStatusTrue(provinceName.toUpperCase());
            }

            if (deliveryZone == null) {
                throw new BadRequestExceptions(Constants.ErrorDeliveryZone);
            }

            if (province == null) {
                throw new BadRequestExceptions(Constants.ErrorProvince);
            }else{
                district = districtRepository.findByNameAndProvinceIdAndStatusTrue(districtName.toUpperCase(),province.getId());
            }

            if(district==null){
                throw new BadRequestExceptions(Constants.ErrorDistrict);
            }else{
                deliveryZoneDistrict = deliveryZoneDistrictRepository.findByDeliveryZoneIdAndDistrictIdAndStatusTrue(deliveryZone.getId(),district.getId());
            }

            if(deliveryZoneDistrict==null){
                throw new BadRequestExceptions(Constants.ErrorDeliveryZoneDistrict);
            }

            try {
                deliveryZoneDistrict.setStatus(false);
                deliveryZoneDistrict.setUpdateDate(OffsetDateTime.now());
                deliveryZoneDistrict.setUser(user);
                deliveryZoneDistrict.setUserId(user.getId());
                deliveryZoneDistrict.setClient(user.getClient());
                deliveryZoneDistrict.setClientId(user.getClientId());
                iAudit.save("DELETE_DELIVERY_ZONE_DISTRICT",
                        "RELACION DISTRITO "+
                                deliveryZoneDistrict.getDistrict().getName()+
                                " CON ZONA DE DELIVERY "+
                                deliveryZoneDistrict.getDeliveryZone().getName()+" ELIMINADA."
                        ,deliveryZoneDistrict.getDeliveryZone().getName() + " - "
                        + deliveryZoneDistrict.getDistrict().getName(),user.getUsername());
                return ResponseDelete.builder()
                        .code(200)
                        .message(Constants.delete)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(String deliveryZoneName, String districtName, String provinceName, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            DeliveryZone deliveryZone;
            District district;
            DeliveryZoneDistrict deliveryZoneDistrict;
            Province province;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user==null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                deliveryZone = deliveryZoneRepository.findByNameAndClientId(deliveryZoneName.toUpperCase(),user.getClientId());
                province = provinceRepository.findByNameAndStatusTrue(provinceName.toUpperCase());
            }

            if (deliveryZone == null) {
                throw new BadRequestExceptions(Constants.ErrorDeliveryZone);
            }

            if (province == null) {
                throw new BadRequestExceptions(Constants.ErrorProvince);
            }else{
                district = districtRepository.findByNameAndProvinceIdAndStatusTrue(districtName.toUpperCase(),province.getId());
            }

            if(district==null){
                throw new BadRequestExceptions(Constants.ErrorDistrict);
            }else{
                deliveryZoneDistrict = deliveryZoneDistrictRepository.findByDeliveryZoneIdAndDistrictIdAndStatusFalse(deliveryZone.getId(),district.getId());
            }

            if(deliveryZoneDistrict==null){
                throw new BadRequestExceptions(Constants.ErrorDeliveryZoneDistrict);
            }

            try {
                deliveryZoneDistrict.setStatus(true);
                deliveryZoneDistrict.setUpdateDate(OffsetDateTime.now());
                deliveryZoneDistrict.setUser(user);
                deliveryZoneDistrict.setUserId(user.getId());
                deliveryZoneDistrict.setClient(user.getClient());
                deliveryZoneDistrict.setClientId(user.getClientId());
                iAudit.save("ACTIVATE_DELIVERY_ZONE_DISTRICT",
                        "RELACION DISTRITO "+
                                deliveryZoneDistrict.getDistrict().getName()+
                                " CON ZONA DE DELIVERY "+
                                deliveryZoneDistrict.getDeliveryZone().getName()+" ACTIVADA."
                        ,deliveryZoneDistrict.getDeliveryZone().getName() + " - "
                                + deliveryZoneDistrict.getDistrict().getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.update)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<DeliveryZoneDistrictDTO>> list(String deliveryZone, String district, OffsetDateTime registrationStartDate, OffsetDateTime registrationEndDate, OffsetDateTime updateStartDate, OffsetDateTime updateEndDate, String sort, String sortColumn, Integer pageNumber, Integer pageSize, Boolean status) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<DeliveryZoneDistrict> deliveryZoneDistrictPage;
            try {
                deliveryZoneDistrictPage = deliveryZoneDistrictRepositoryCustom.searchForDeliveryZoneDistrict(
                        deliveryZone,
                        district,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateEndDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        status);
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if (deliveryZone.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }
            List<DeliveryZoneDistrictDTO> deliveryZoneDistrictDTOS = deliveryZoneDistrictPage.getContent().stream().map(deliveryZoneDistrict -> DeliveryZoneDistrictDTO.builder()
                    .id(deliveryZoneDistrict.getId())
                    .updateDate(deliveryZoneDistrict.getUpdateDate())
                    .deliveryZone(deliveryZoneDistrict.getDeliveryZone().getName())
                    .district(deliveryZoneDistrict.getDistrict().getName())
                    .registrationDate(deliveryZoneDistrict.getRegistrationDate())
                    .user(deliveryZoneDistrict.getUser().getUsername())
                    .status(deliveryZoneDistrict.getStatus())
                    .province(deliveryZoneDistrict.getDistrict().getProvince().getName())
                    .build()).toList();
            return new PageImpl<>(deliveryZoneDistrictDTOS,
                    deliveryZoneDistrictPage.getPageable(), deliveryZoneDistrictPage.getTotalElements());
        });
    }
}

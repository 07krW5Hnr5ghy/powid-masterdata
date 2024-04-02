package com.proyect.masterdata.services.impl;

import java.util.Collections;
import java.util.Date;
import java.util.List;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.repository.*;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.dto.SupplierDTO;
import com.proyect.masterdata.dto.request.RequestSupplier;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.ISupplier;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class SupplierImpl implements ISupplier {
    private final UserRepository userRepository;
    private final SupplierRepository supplierRepository;
    private final SupplierRepositoryCustom supplierRepositoryCustom;
    private final SupplierTypeRepository supplierTypeRepository;
    private final DistrictRepository districtRepository;
    private final CountryRepository countryRepository;
    @Override
    public ResponseSuccess save(RequestSupplier requestSupplier, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        Supplier supplierRuc;
        Supplier supplierName;
        SupplierType supplierType;
        District district;
        Country country;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            supplierType = supplierTypeRepository.findByNameAndStatusTrue(requestSupplier.getSupplierType().toUpperCase());
            district = districtRepository.findByNameAndStatusTrue(requestSupplier.getDistrict().toUpperCase());
            country = countryRepository.findByNameAndStatusTrue(requestSupplier.getCountry().toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }else{
            supplierRuc = supplierRepository.findByRucAndClientId(requestSupplier.getRuc(), user.getClientId());
            supplierName = supplierRepository.findByBusinessNameAndClientId(requestSupplier.getBusinessName().toUpperCase(), user.getClientId());
        }

        if (supplierRuc != null) {
            throw new BadRequestExceptions(Constants.ErrorSupplierRucExists);
        }

        if(supplierName != null){
            throw new BadRequestExceptions(Constants.ErrorSupplierNameExists);
        }

        if(supplierType == null){
            throw new BadRequestExceptions(Constants.ErrorSupplierType);
        }

        if(district == null){
            throw new BadRequestExceptions(Constants.ErrorDistrict);
        }

        if(country == null){
            throw new BadRequestExceptions(Constants.ErrorCountry);
        }

        try {
            supplierRepository.save(Supplier.builder()
                    .businessName(requestSupplier.getBusinessName().toUpperCase())
                    .client(user.getClient())
                    .clientId(user.getClientId())
                    .country(country)
                    .countryId(country.getId())
                    .email(requestSupplier.getEmail())
                    .location(requestSupplier.getLocation().toUpperCase())
                    .phoneNumber(requestSupplier.getPhoneNumber())
                    .ruc(requestSupplier.getRuc())
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .status(true)
                    .supplierType(supplierType)
                    .supplierTypeId(supplierType.getId())
                    .district(district)
                    .districtId(district.getId())
                    .tokenUser(user.getUsername().toUpperCase())
                    .build());

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
    public ResponseDelete delete(String ruc, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        Supplier supplier;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }else {
            supplier = supplierRepository.findByRucAndClientIdAndStatusTrue(ruc, user.getClientId());
        }

        if (supplier == null) {
            throw new BadRequestExceptions(Constants.ErrorSupplier);
        }

        try {
            supplier.setStatus(false);
            supplier.setUpdateDate(new Date(System.currentTimeMillis()));
            supplierRepository.save(supplier);

            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public Page<SupplierDTO> list(String name, String ruc, String user, String sort, String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {

        Page<Supplier> supplierPage;
        Long clientId;

        try {
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
            supplierPage = supplierRepositoryCustom.searchForSupplier(name, ruc, clientId, sort, sortColumn, pageNumber,
                    pageSize, true);
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if (supplierPage.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }

        List<SupplierDTO> supplierDTOs = supplierPage.getContent().stream().map(supplier -> SupplierDTO.builder()
                .businessName(supplier.getBusinessName())
                .country(supplier.getCountry().getName())
                .email(supplier.getEmail())
                .location(supplier.getLocation())
                .phoneNumber(supplier.getPhoneNumber())
                .ruc(supplier.getRuc())
                .build()).toList();

        return new PageImpl<>(supplierDTOs, supplierPage.getPageable(), supplierPage.getTotalElements());

    }

    @Override
    public List<SupplierDTO> listSuppliers(String user) throws InternalErrorExceptions, BadRequestExceptions {
        Long clientId;
        List<Supplier> suppliers;
        try{
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
            suppliers = supplierRepository.findAllByClientIdAndStatusTrue(clientId);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(suppliers.isEmpty()){
            return Collections.emptyList();
        }

        return suppliers.stream().map(supplier -> SupplierDTO.builder()
                .id(supplier.getId())
                .businessName(supplier.getBusinessName())
                .country(supplier.getCountry().getName())
                .email(supplier.getEmail())
                .location(supplier.getLocation())
                .phoneNumber(supplier.getPhoneNumber())
                .ruc(supplier.getRuc())
                .department(supplier.getDistrict().getProvince().getDepartment().getName())
                .province(supplier.getDistrict().getProvince().getName())
                .district(supplier.getDistrict().getName())
                .supplierType(supplier.getSupplierType().getName())
                .build()).toList();
    }

    @Override
    public List<SupplierDTO> listSuppliersFalse(String user) throws InternalErrorExceptions, BadRequestExceptions {
        Long clientId;
        List<Supplier> suppliers;
        try{
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
            suppliers = supplierRepository.findAllByClientIdAndStatusFalse(clientId);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(suppliers.isEmpty()){
            return Collections.emptyList();
        }

        return suppliers.stream().map(supplier -> SupplierDTO.builder()
                .id(supplier.getId())
                .businessName(supplier.getBusinessName())
                .country(supplier.getCountry().getName())
                .email(supplier.getEmail())
                .location(supplier.getLocation())
                .phoneNumber(supplier.getPhoneNumber())
                .ruc(supplier.getRuc())
                .department(supplier.getDistrict().getProvince().getDepartment().getName())
                .province(supplier.getDistrict().getProvince().getName())
                .district(supplier.getDistrict().getName())
                .supplierType(supplier.getSupplierType().getName())
                .build()).toList();
    }

}

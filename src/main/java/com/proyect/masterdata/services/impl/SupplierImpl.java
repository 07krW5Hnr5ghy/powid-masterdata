package com.proyect.masterdata.services.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IAudit;
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
    private final IAudit iAudit;
    private final DepartmentRepository departmentRepository;
    private final ProvinceRepository provinceRepository;
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
            Supplier newSupplier = supplierRepository.save(Supplier.builder()
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
            iAudit.save("ADD_SUPPLIER","ADD SUPPLIER WITH RUC "+newSupplier.getRuc()+".",user.getUsername());
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
    public CompletableFuture<ResponseSuccess> saveAsync(RequestSupplier requestSupplier, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
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
                Supplier newSupplier = supplierRepository.save(Supplier.builder()
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
                iAudit.save("ADD_SUPPLIER","ADD SUPPLIER WITH RUC "+newSupplier.getRuc()+".",user.getUsername());
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
    public CompletableFuture<ResponseDelete> delete(String ruc, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
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
                supplier.setTokenUser(user.getUsername());
                supplierRepository.save(supplier);
                iAudit.save("DELETE_SUPPLIER","DELETE SUPPLIER WITH RUC "+supplier.getRuc()+".",user.getUsername());
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
    public CompletableFuture<ResponseSuccess> activate(String ruc, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
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
                supplier = supplierRepository.findByRucAndClientIdAndStatusFalse(ruc, user.getClientId());
            }

            if (supplier == null) {
                throw new BadRequestExceptions(Constants.ErrorSupplier);
            }

            try {
                supplier.setStatus(true);
                supplier.setUpdateDate(new Date(System.currentTimeMillis()));
                supplier.setTokenUser(user.getUsername());
                supplierRepository.save(supplier);
                iAudit.save("ACTIVATE_SUPPLIER","ACTIVATE SUPPLIER WITH RUC "+supplier.getRuc()+".",user.getUsername());
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
    public CompletableFuture<Page<SupplierDTO>> list(
            String user,
            List<String> names,
            List<String> rucs,
            List<String> countries,
            List<String> supplierTypes,
            List<String> departments,
            List<String> provinces,
            List<String> districts,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<Supplier> supplierPage;
            Long clientId;
            List<String> namesUppercase;
            List<String> rucsUppercase;
            List<Long> countryIds;
            List<Long> supplierTypeIds;
            List<Long> departmentIds;
            List<Long> provinceIds;
            List<Long> districtIds;

            if(names != null && !names.isEmpty()){
                namesUppercase = names.stream().map(String::toUpperCase).toList();
            }else{
                namesUppercase = new ArrayList<>();
            }

            if(rucs != null && !rucs.isEmpty()){
                rucsUppercase = rucs.stream().map(String::toUpperCase).toList();
            }else{
                rucsUppercase = new ArrayList<>();
            }

            if(countries != null && !countries.isEmpty()){
                countryIds = countryRepository.findByNameIn(
                        countries.stream().map(String::toUpperCase).toList()
                ).stream().map(Country::getId).toList();
            }else{
                countryIds = new ArrayList<>();
            }

            if(supplierTypes != null && !supplierTypes.isEmpty()){
                supplierTypeIds = supplierTypeRepository.findByNameIn(
                        supplierTypes.stream().map(String::toUpperCase).toList()
                ).stream().map(SupplierType::getId).toList();
            }else{
                supplierTypeIds = new ArrayList<>();
            }

            if(departments != null && !departments.isEmpty()){
                departmentIds = departmentRepository.findByNameIn(
                        departments.stream().map(String::toUpperCase).toList()
                ).stream().map(Department::getId).toList();
            }else{
                departmentIds = new ArrayList<>();
            }

            if(provinces != null && !provinces.isEmpty()){
                provinceIds = provinceRepository.findByNameIn(
                        provinces.stream().map(String::toUpperCase).toList()
                ).stream().map(Province::getId).toList();
            }else{
                provinceIds = new ArrayList<>();
            }

            if(districts != null && !districts.isEmpty()){
                districtIds = districtRepository.findByNameIn(
                        districts.stream().map(String::toUpperCase).toList()
                ).stream().map(District::getId).toList();
            }else{
                districtIds = new ArrayList<>();
            }

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                supplierPage = supplierRepositoryCustom.searchForSupplier(
                        clientId,
                        namesUppercase,
                        rucsUppercase,
                        countryIds,
                        supplierTypeIds,
                        departmentIds,
                        provinceIds,
                        districtIds,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        true);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (supplierPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<SupplierDTO> supplierDTOs = supplierPage.getContent().stream().map(supplier -> SupplierDTO.builder()
                    .name(supplier.getBusinessName())
                    .country(supplier.getCountry().getName())
                    .email(supplier.getEmail())
                    .location(supplier.getLocation())
                    .phone(supplier.getPhoneNumber())
                    .ruc(supplier.getRuc())
                    .department(supplier.getDistrict().getProvince().getDepartment().getName())
                    .province(supplier.getDistrict().getProvince().getName())
                    .district(supplier.getDistrict().getName())
                    .supplierType(supplier.getSupplierType().getName())
                    .registrationDate(supplier.getRegistrationDate())
                    .updateDate(supplier.getUpdateDate())
                    .build()).toList();

            return new PageImpl<>(supplierDTOs, supplierPage.getPageable(), supplierPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<SupplierDTO>> listSuppliers(String user) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
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
                    .name(supplier.getBusinessName())
                    .country(supplier.getCountry().getName())
                    .email(supplier.getEmail())
                    .location(supplier.getLocation())
                    .phone(supplier.getPhoneNumber())
                    .ruc(supplier.getRuc())
                    .department(supplier.getDistrict().getProvince().getDepartment().getName())
                    .province(supplier.getDistrict().getProvince().getName())
                    .district(supplier.getDistrict().getName())
                    .supplierType(supplier.getSupplierType().getName())
                    .registrationDate(supplier.getRegistrationDate())
                    .updateDate(supplier.getUpdateDate())
                    .build()).toList();
        });
    }

    @Override
    public CompletableFuture<List<SupplierDTO>> listSuppliersFalse(String user) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
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
                    .name(supplier.getBusinessName())
                    .country(supplier.getCountry().getName())
                    .email(supplier.getEmail())
                    .location(supplier.getLocation())
                    .phone(supplier.getPhoneNumber())
                    .ruc(supplier.getRuc())
                    .department(supplier.getDistrict().getProvince().getDepartment().getName())
                    .province(supplier.getDistrict().getProvince().getName())
                    .district(supplier.getDistrict().getName())
                    .supplierType(supplier.getSupplierType().getName())
                    .registrationDate(supplier.getRegistrationDate())
                    .updateDate(supplier.getUpdateDate())
                    .build()).toList();
        });
    }

    @Override
    public CompletableFuture<List<SupplierDTO>> listSuppliersFilter(String user) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Long clientId;
            List<Supplier> suppliers;
            try{
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                suppliers = supplierRepository.findAllByClientId(clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(suppliers.isEmpty()){
                return Collections.emptyList();
            }

            return suppliers.stream().map(supplier -> SupplierDTO.builder()
                    .name(supplier.getBusinessName())
                    .country(supplier.getCountry().getName())
                    .email(supplier.getEmail())
                    .location(supplier.getLocation())
                    .phone(supplier.getPhoneNumber())
                    .ruc(supplier.getRuc())
                    .department(supplier.getDistrict().getProvince().getDepartment().getName())
                    .province(supplier.getDistrict().getProvince().getName())
                    .district(supplier.getDistrict().getName())
                    .supplierType(supplier.getSupplierType().getName())
                    .registrationDate(supplier.getRegistrationDate())
                    .updateDate(supplier.getUpdateDate())
                    .build()).toList();
        });
    }
}

package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Brand;
import com.proyect.masterdata.domain.Membership;
import com.proyect.masterdata.domain.MembershipState;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.BrandDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IBrand;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class BrandImpl implements IBrand {

    private final UserRepository userRepository;
    private final BrandRepository brandRepository;
    private final BrandRepositoryCustom brandRepositoryCustom;
    private final MembershipRepository membershipRepository;
    private final MembershipStateRepository membershipStateRepository;
    private final IAudit iAudit;
    @Override
    public ResponseSuccess save(String name, String tokenUser, String sku) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        Brand brand;
        Membership membership;
        MembershipState membershipState;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            membershipState = membershipStateRepository.findByNameAndStatusTrue("ACTIVA");
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }else{
            brand = brandRepository.findByNameOrSkuAndClientId(name.toUpperCase(),sku.toUpperCase(),user.getClientId());
            //membership = membershipRepository.findByClientIdAndMembershipStateId(user.getClientId(), membershipState.getId());
        }

        //if(membership == null){
            //throw new BadRequestExceptions(Constants.ErrorMembershipExpired);
        //}

        if (brand!=null) {
            throw new BadRequestExceptions(Constants.ErrorBrandExists);
        }

        try {
            Brand newBrand = brandRepository.save(Brand.builder()
                    .name(name.toUpperCase())
                    .sku(sku.toUpperCase())
                    .status(true)
                    .registrationDate(OffsetDateTime.now())
                    .updateDate(OffsetDateTime.now())
                    .user(user)
                    .userId(user.getId())
                    .client(user.getClient())
                    .clientId(user.getClientId())
                    .build());
            iAudit.save("ADD_BRAND","MARCA "+newBrand.getName()+" CREADA.", newBrand.getName(), user.getUsername());
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
    public CompletableFuture<ResponseSuccess> saveAsync(String name, String tokenUser, String sku) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(() -> {
            User user;
            Brand brand;
            Membership membership;
            MembershipState membershipState;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                membershipState = membershipStateRepository.findByNameAndStatusTrue("ACTIVA");
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                brand = brandRepository.findByNameOrSkuAndClientId(name.toUpperCase(),sku.toUpperCase(),user.getClientId());
                //membership = membershipRepository.findByClientIdAndMembershipStateId(user.getClientId(), membershipState.getId());
            }

            //if(membership == null){
                //throw new BadRequestExceptions(Constants.ErrorMembershipExpired);
            //}

            if (brand!=null) {
                throw new BadRequestExceptions(Constants.ErrorBrandExists);
            }

            try {
                Brand newBrand = brandRepository.save(Brand.builder()
                        .name(name.toUpperCase())
                        .status(true)
                        .sku(sku.toUpperCase())
                        .registrationDate(OffsetDateTime.now())
                                .updateDate(OffsetDateTime.now())
                                .user(user)
                                .userId(user.getId())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .build());
                iAudit.save("ADD_BRAND","MARCA "+newBrand.getName()+" CREADA.",newBrand.getName(),user.getUsername());
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
    public CompletableFuture<ResponseDelete> delete(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(() -> {
            User user;
            Brand brand;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                brand = brandRepository.findByNameAndClientIdAndStatusTrue(name.toUpperCase(),user.getClientId());
            }

            if (brand == null) {
                throw new BadRequestExceptions(Constants.ErrorBrand);
            }

            try {
                brand.setStatus(false);
                brand.setUpdateDate(OffsetDateTime.now());
                brand.setUser(user);
                brand.setUserId(user.getId());
                brandRepository.save(brand);
                iAudit.save("DELETE_BRAND","MARCA "+brand.getName()+" DESACTIVADA.", brand.getName(), user.getUsername());
                return ResponseDelete.builder()
                        .code(200)
                        .message(Constants.delete)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<BrandDTO>> listPagination(
            String tokenUser,
            List<String> names,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<Brand> brandPage;
            List<String> brandsUppercase;
            UUID clientId;

            if(names != null && !names.isEmpty()){
                brandsUppercase = names.stream().map(String::toUpperCase).toList();
            }else{
                brandsUppercase = new ArrayList<>();
            }

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase()).getClient().getId();
                brandPage = brandRepositoryCustom.searchForBrand(
                        clientId,
                        brandsUppercase,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateEndDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        true);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (brandPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<BrandDTO> brandDTOs = brandPage.getContent().stream().map(brand -> BrandDTO.builder()
                    .status(brand.getStatus())
                    .name(brand.getName())
                    .sku(brand.getSku())
                    .client(brand.getClient().getBusiness())
                    .registrationDate(brand.getRegistrationDate())
                    .updateDate(brand.getUpdateDate())
                    .id(brand.getId())
                    .tokenUser(brand.getUser().getUsername())
                    .build()).toList();

            return new PageImpl<>(brandDTOs, brandPage.getPageable(),
                    brandPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<Page<BrandDTO>> listStatusFalse(
            String tokenUser,
            List<String> names,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(() -> {
            Page<Brand> brandPage;
            List<String> brandsUppercase;
            UUID clientId;

            if(names != null && !names.isEmpty()){
                brandsUppercase = names.stream().map(String::toUpperCase).toList();
            }else{
                brandsUppercase = new ArrayList<>();
            }

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase()).getClient().getId();
                brandPage = brandRepositoryCustom.searchForBrand(
                        clientId,
                        brandsUppercase,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateEndDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        false);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (brandPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<BrandDTO> brandDTOs = brandPage.getContent().stream().map(brand -> BrandDTO.builder()
                    .status(brand.getStatus())
                    .id(brand.getId())
                    .sku(brand.getSku())
                    .name(brand.getName())
                    .client(brand.getClient().getBusiness())
                    .registrationDate(brand.getRegistrationDate())
                    .updateDate(brand.getUpdateDate())
                    .tokenUser(brand.getUser().getUsername())
                    .build()).toList();

            return new PageImpl<>(brandDTOs, brandPage.getPageable(),
                    brandPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Brand brand;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                brand = brandRepository.findByNameAndClientIdAndStatusFalse(name.toUpperCase(),user.getClientId());
            }

            if (brand == null) {
                throw new BadRequestExceptions(Constants.ErrorBrand);
            }

            try {
                brand.setStatus(true);
                brand.setUpdateDate(OffsetDateTime.now());
                brand.setUser(user);
                brand.setUserId(user.getId());
                brandRepository.save(brand);
                iAudit.save("ACTIVATE_BRAND","MARCA "+brand.getName()+" ACTIVADA.", brand.getName(), user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.update)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<List<BrandDTO>> listBrands(String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(() -> {
            List<Brand> brands;
            UUID clientId;

            try{
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                brands = brandRepository.findAllByClientIdAndStatusTrue(clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(brands.isEmpty()){
                return Collections.emptyList();
            }

            return brands.stream().map(brand -> BrandDTO.builder()
                    .id(brand.getId())
                    .name(brand.getName())
                    .sku(brand.getSku())
                    .client(brand.getClient().getBusiness())
                    .registrationDate(brand.getRegistrationDate())
                    .updateDate(brand.getUpdateDate())
                    .tokenUser(brand.getUser().getUsername())
                    .build()).toList();
        });
    }

    @Override
    public CompletableFuture<List<BrandDTO>> listBrandsFalse(String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Brand> brands;
            UUID clientId;

            try{
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                brands = brandRepository.findAllByClientIdAndStatusFalse(clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(brands.isEmpty()){
                return Collections.emptyList();
            }

            return brands.stream().map(brand -> BrandDTO.builder()
                    .status(brand.getStatus())
                    .id(brand.getId())
                    .name(brand.getName())
                    .sku(brand.getSku())
                    .client(brand.getClient().getBusiness())
                    .registrationDate(brand.getRegistrationDate())
                    .updateDate(brand.getUpdateDate())
                    .tokenUser(brand.getUser().getUsername())
                    .build()).toList();
        });
    }

    @Override
    public CompletableFuture<List<BrandDTO>> listFilter(String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Brand> brands;
            UUID clientId;

            try{
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                brands = brandRepository.findAllByClientId(clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(brands.isEmpty()){
                return Collections.emptyList();
            }

            return brands.stream().map(brand -> BrandDTO.builder()
                    .status(brand.getStatus())
                    .id(brand.getId())
                    .name(brand.getName())
                    .sku(brand.getSku())
                    .client(brand.getClient().getBusiness())
                    .registrationDate(brand.getRegistrationDate())
                    .updateDate(brand.getUpdateDate())
                    .tokenUser(brand.getUser().getUsername())
                    .build()).toList();
        });
    }

}

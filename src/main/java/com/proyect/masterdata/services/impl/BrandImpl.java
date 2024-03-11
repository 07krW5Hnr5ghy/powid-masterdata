package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Membership;
import com.proyect.masterdata.domain.MembershipState;
import com.proyect.masterdata.repository.*;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.Brand;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.BrandDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.BrandMapper;
import com.proyect.masterdata.services.IBrand;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import java.util.Collections;
import java.util.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
@Log4j2
public class BrandImpl implements IBrand {

    private final UserRepository userRepository;
    private final BrandRepository brandRepository;
    private final BrandRepositoryCustom brandRepositoryCustom;
    private final MembershipRepository membershipRepository;
    private final MembershipStateRepository membershipStateRepository;
    @Override
    public ResponseSuccess save(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        boolean existsBrand;
        Membership membership;
        MembershipState membershipState;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            existsBrand = brandRepository.existsByName(name.toUpperCase());
            membershipState = membershipStateRepository.findByNameAndStatusTrue("ACTIVA");
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }else{
            membership = membershipRepository.findByClientIdAndMembershipStateId(user.getClientId(), membershipState.getId());
        }

        if(membership == null){
            //throw new BadRequestExceptions(Constants.ErrorMembershipExpired);
        }

        if (existsBrand) {
            throw new BadRequestExceptions(Constants.ErrorBrandExists);
        }

        try {
            brandRepository.save(Brand.builder()
                    .name(name.toUpperCase())
                    .status(true)
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .tokenUser(tokenUser.toUpperCase())
                    .client(user.getClient())
                    .clientId(user.getClientId())
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
    public ResponseSuccess saveAll(List<String> namesList, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        List<Brand> brandList;
        Membership membership;
        MembershipState membershipState;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            brandList = brandRepository.findByNameIn(namesList);
            membershipState = membershipStateRepository.findByNameAndStatusTrue("ACTIVA");
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }else{
            membership = membershipRepository.findByClientIdAndMembershipStateId(user.getClientId(), membershipState.getId());
        }

        if(membership == null){
            throw new BadRequestExceptions(Constants.ErrorMembershipExpired);
        }

        if (!brandList.isEmpty()) {
            throw new BadRequestExceptions(Constants.ErrorBrandExists);
        }

        try {
            brandRepository.saveAll(namesList.stream().map(name -> Brand.builder()
                    .name(name.toUpperCase())
                    .status(true)
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .tokenUser(tokenUser.toUpperCase())
                    .client(user.getClient())
                    .clientId(user.getClientId())
                    .build()).toList());
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
    public ResponseDelete delete(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        Brand brand;
        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            brand = brandRepository.findByNameAndStatusTrue(name.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (brand == null) {
            throw new BadRequestExceptions(Constants.ErrorBrand);
        }

        try {
            brand.setStatus(false);
            brand.setUpdateDate(new Date(System.currentTimeMillis()));
            brand.setTokenUser(tokenUser.toUpperCase());
            brandRepository.save(brand);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public Page<BrandDTO> list(String name, String tokenUser, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions {

        Page<Brand> brandPage;
        Long clientId;

        try {
            clientId = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase()).getClient().getId();
            brandPage = brandRepositoryCustom.searchForBrand(name, clientId, sort, sortColumn, pageNumber, pageSize, true);
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if (brandPage.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }

        List<BrandDTO> brandDTOs = brandPage.getContent().stream().map(brand -> BrandDTO.builder()
                .name(brand.getName())
                .client(brand.getClient().getBusiness())
                .tokenUser(brand.getTokenUser())
                .build()).toList();

        return new PageImpl<>(brandDTOs, brandPage.getPageable(),
                brandPage.getTotalElements());
    }

    @Override
    public Page<BrandDTO> listStatusFalse(String name, String tokenUser, String sort, String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions {

        Page<Brand> brandPage;
        Long clientId;

        try {
            clientId = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase()).getClient().getId();
            brandPage = brandRepositoryCustom.searchForBrand(name, clientId, sort, sortColumn, pageNumber, pageSize,
                    false);
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if (brandPage.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }

        List<BrandDTO> brandDTOs = brandPage.getContent().stream().map(brand -> BrandDTO.builder()
                .name(brand.getName())
                .client(brand.getClient().getBusiness())
                .tokenUser(brand.getTokenUser())
                .build()).toList();

        return new PageImpl<>(brandDTOs, brandPage.getPageable(),
                brandPage.getTotalElements());
    }

    @Override
    public ResponseSuccess activate(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        Brand brand;
        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            brand = brandRepository.findByNameAndStatusFalse(name.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (brand == null) {
            throw new BadRequestExceptions(Constants.ErrorBrand);
        }

        try {
            brand.setStatus(true);
            brand.setUpdateDate(new Date(System.currentTimeMillis()));
            brand.setTokenUser(tokenUser.toUpperCase());
            brandRepository.save(brand);
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.update)
                    .build();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
    }

}

package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Country;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.CountryDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.CountryRepository;
import com.proyect.masterdata.repository.CountryRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.ICountry;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class CountryImpl implements ICountry {
    private final UserRepository userRepository;
    private final CountryRepository countryRepository;
    private final CountryRepositoryCustom countryRepositoryCustom;
    @Override
    public CompletableFuture<ResponseSuccess> save(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Country country;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                country = countryRepository.findByNameAndStatusTrue(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(country != null){
                throw new BadRequestExceptions(Constants.ErrorCountry);
            }

            try {
                countryRepository.save(Country.builder()
                        .name(name.toUpperCase())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .status(true)
                        .tokenUser(user.getUsername())
                        .build());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<CountryDTO>> listCountry(String name, String user, String sort, String sortColumn, Integer pageNumber,
                                        Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<Country> countryPage;
            try {
                countryPage = countryRepositoryCustom.searchForCountry(name,user,sort,sortColumn,pageNumber,pageSize,true);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if (countryPage.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }

            List<CountryDTO> countryDTOS = countryPage.getContent().stream().map(country -> CountryDTO.builder()
                    .id(country.getId().toString())
                    .value(country.getName())
                    .build()).toList();

            return new PageImpl<>(countryDTOS,countryPage.getPageable(),countryPage.getTotalElements());
        });
    }
}

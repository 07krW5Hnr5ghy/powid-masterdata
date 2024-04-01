package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Country;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.CountryDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.CountryRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.ICountry;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
@Log4j2
public class CountryImpl implements ICountry {
    private final UserRepository userRepository;
    private final CountryRepository countryRepository;
    @Override
    public ResponseSuccess save(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
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

    }

    @Override
    public List<CountryDTO> listCountry() throws BadRequestExceptions {
        List<Country> countries = new ArrayList<>();
        try {
            countries = countryRepository.findAllByStatusTrue();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if (countries.isEmpty()){
            return Collections.emptyList();
        }

        return countries.stream().map(country -> CountryDTO.builder()
                .id(country.getId().toString())
                .value(country.getName())
                .build()).toList();
    }
}

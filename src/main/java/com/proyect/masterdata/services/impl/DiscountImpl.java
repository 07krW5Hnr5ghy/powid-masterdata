package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Discount;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.DiscountRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IDiscount;
import com.proyect.masterdata.utils.Constants;
import lombok.AllArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@AllArgsConstructor
@Log4j2
public class DiscountImpl implements IDiscount {
    private final UserRepository userRepository;
    private final DiscountRepository discountRepository;
    @Override
    public ResponseSuccess save(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        User user;
        Discount discount;
        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            discount = discountRepository.findByName(name.toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
        if(user==null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }
        if(discount!=null){
            throw new BadRequestExceptions(Constants.ErrorDiscountExist);
        }
        try{
            discountRepository.save(Discount.builder()
                            .name(name.toUpperCase())
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .updateDate(new Date(System.currentTimeMillis()))
                            .status(true)
                    .build());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<ResponseSuccess> saveAsync(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Discount discount;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                discount = discountRepository.findByName(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(discount!=null){
                throw new BadRequestExceptions(Constants.ErrorDiscountExist);
            }
            try{
                discountRepository.save(Discount.builder()
                        .name(name.toUpperCase())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .updateDate(new Date(System.currentTimeMillis()))
                        .status(true)
                        .build());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<List<String>> listDiscount() throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Discount> discountList;
            try{
                discountList = discountRepository.findAllByStatusTrue();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            return discountList.stream().map(Discount::getName).toList();
        });
    }
}

package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.MembershipState;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.MembershipStateRepository;
import com.proyect.masterdata.repository.MembershipStateRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IMembershipState;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class MembershipStateImpl implements IMembershipState {
    private final MembershipStateRepository membershipStateRepository;
    private final UserRepository userRepository;
    private final MembershipStateRepositoryCustom membershipStateRepositoryCustom;
    @Override
    public CompletableFuture<ResponseSuccess> save(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            MembershipState membershipState;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                membershipState = membershipStateRepository.findByName(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(membershipState != null){
                throw new BadRequestExceptions(Constants.ErrorMembershipStateExists);
            }

            try{
                membershipStateRepository.save(MembershipState.builder()
                        .name(name.toUpperCase())
                        .status(true)
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .updateDate(new Date(System.currentTimeMillis()))
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
    public CompletableFuture<Page<String>> list(String name, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<MembershipState> membershipStatePage;
            try{
                membershipStatePage = membershipStateRepositoryCustom.searchForMembershipState(name,sort,sortColumn,pageNumber,pageSize,true);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if(membershipStatePage.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }
            List<String> membershipStateDTOs = membershipStatePage.getContent().stream().map(MembershipState::getName).toList();
            return new PageImpl<>(membershipStateDTOs,membershipStatePage.getPageable(),membershipStatePage.getTotalElements());
        });
    }
}

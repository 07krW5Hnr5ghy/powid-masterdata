package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.MembershipState;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.MembershipStateDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.MembershipStateRepository;
import com.proyect.masterdata.repository.MembershipStateRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
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
    private final IAudit iAudit;
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
                MembershipState newMembershipState = membershipStateRepository.save(MembershipState.builder()
                        .name(name.toUpperCase())
                        .status(true)
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .updateDate(new Date(System.currentTimeMillis()))
                                .tokenUser(user.getUsername())
                        .build());
                iAudit.save("ADD_MEMBERSHIP_STATE","ESTADO DE MEMBRESIA "+newMembershipState.getName()+" CREADO.",newMembershipState.getName(),user.getUsername());
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
    public CompletableFuture<Page<MembershipStateDTO>> listFalse(String name, Date registrationStartDate, Date registrationEndDate, Date updateStartDate, Date updateEndDate, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<MembershipState> membershipStatePage;
            try{
                membershipStatePage = membershipStateRepositoryCustom.searchForMembershipState(
                        name,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateEndDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        false);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if(membershipStatePage.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }
            List<MembershipStateDTO> membershipStateDTOs = membershipStatePage.getContent().stream().map(membershipState -> MembershipStateDTO.builder()
                    .name(membershipState.getName())
                    .registrationDate(membershipState.getRegistrationDate())
                    .updateDate(membershipState.getUpdateDate())
                    .build()).toList();
            return new PageImpl<>(membershipStateDTOs,membershipStatePage.getPageable(),membershipStatePage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<Page<MembershipStateDTO>> listPagination(
            String name,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<MembershipState> membershipStatePage;
            try{
                membershipStatePage = membershipStateRepositoryCustom.searchForMembershipState(
                        name,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateEndDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        true);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if(membershipStatePage.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }
            List<MembershipStateDTO> membershipStateDTOs = membershipStatePage.getContent().stream().map(membershipState -> MembershipStateDTO.builder()
                    .name(membershipState.getName())
                    .registrationDate(membershipState.getRegistrationDate())
                    .updateDate(membershipState.getUpdateDate())
                    .build()).toList();
            return new PageImpl<>(membershipStateDTOs,membershipStatePage.getPageable(),membershipStatePage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            MembershipState membershipState;
            User user;
            try {
                user = userRepository.findByUsernameAndStatusTrue(name.toUpperCase());
                membershipState = membershipStateRepository.findByNameAndStatusTrue(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(membershipState==null){
                throw new BadRequestExceptions(Constants.ErrorMembershipState);
            }
            try {
                membershipState.setStatus(false);
                membershipState.setRegistrationDate(new Date(System.currentTimeMillis()));
                membershipState.setTokenUser(user.getUsername());
                membershipStateRepository.save(membershipState);
                iAudit.save("DELETE_MEMBERSHIP_STATE","ESTADO DE MEMBRESIA "+membershipState.getName()+" DESACTIVADO.",membershipState.getName(),user.getUsername());
                return ResponseDelete.builder()
                        .message(Constants.delete)
                        .code(200)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            MembershipState membershipState;
            User user;
            try {
                user = userRepository.findByUsernameAndStatusTrue(name.toUpperCase());
                membershipState = membershipStateRepository.findByNameAndStatusFalse(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(membershipState==null){
                throw new BadRequestExceptions(Constants.ErrorMembershipState);
            }
            try {
                membershipState.setStatus(true);
                membershipState.setRegistrationDate(new Date(System.currentTimeMillis()));
                membershipState.setTokenUser(user.getUsername());
                membershipStateRepository.save(membershipState);
                iAudit.save("ACTIVATE_MEMBERSHIP_STATE","ESTADO DE MEMBRESIA "+membershipState.getName()+" ACTIVADO.",membershipState.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .message(Constants.update)
                        .code(200)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }
}

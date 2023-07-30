package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.LogEvent;
import com.proyect.masterdata.domain.Membership;
import com.proyect.masterdata.dto.MasterListDTO;
import com.proyect.masterdata.dto.response.ResponseMasterList;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.MembershipMapper;
import com.proyect.masterdata.repository.MembershipRepository;
import com.proyect.masterdata.services.IMasterList;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.sql.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
public class MembershipImpl implements IMasterList {
    private final MembershipRepository membershipRepository;
    private final MembershipMapper membershipMapper;

    @Override
    public List<MasterListDTO> listRecords() throws BadRequestExceptions {
        return membershipMapper.membershipListToMembershipListDTO(membershipRepository.findAll());
    }

    @Override
    public ResponseMasterList addRecord(String name) throws BadRequestExceptions {
        try{
            membershipRepository.save(Membership.builder().name(name).status(true).build());
            return ResponseMasterList.builder()
                    .code(200)
                    .message("Success")
                    .build();
        }catch(RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }

    @Override
    public ResponseMasterList deleteRecord(Long id) throws BadRequestExceptions {
        try{
            Membership membership = membershipRepository.findById(id).get();
            membershipRepository.save(Membership.builder()
                    .name(membership.getName())
                    .dateRegistration(new Date(System.currentTimeMillis()))
                    .id(membership.getId())
                    .status(false)
                    .build()
            );
            return ResponseMasterList.builder()
                    .code(200)
                    .message("Success")
                    .build();
        }catch (RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }

    @Override
    public MasterListDTO updateRecord(String name, Long id) throws BadRequestExceptions {
        try{
            Membership membership = membershipRepository.save(Membership.builder()
                    .id(id)
                    .dateRegistration(new Date(System.currentTimeMillis()))
                    .name(name)
                    .status(true)
                    .build()
            );
            return membershipMapper.INSTANCE.membershipToMembershipDTO(membership);
        }catch (RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }
}

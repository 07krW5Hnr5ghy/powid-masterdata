package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.MembershipType;
import com.proyect.masterdata.dto.MasterListDTO;
import com.proyect.masterdata.dto.response.ResponseMasterList;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.MembershipTypeMapper;
import com.proyect.masterdata.repository.MembershipTypeRepository;
import com.proyect.masterdata.services.IMasterList;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.sql.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
public class MembershipTypeImpl implements IMasterList {
    private final MembershipTypeRepository membershipTypeRepository;
    private final MembershipTypeMapper membershipTypeMapper;

    @Override
    public List<MasterListDTO> listRecords() throws BadRequestExceptions {
        return membershipTypeMapper.membershipTypeListToMembershipTypeListDTO(membershipTypeRepository.findAll());
    }

    @Override
    public ResponseMasterList addRecord(String name) throws BadRequestExceptions {
        try{
            membershipTypeRepository.save(MembershipType.builder().name(name).status(true).build());
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
            MembershipType membershipType = membershipTypeRepository.findById(id).get();
            membershipTypeRepository.save(MembershipType.builder()
                    .name(membershipType.getName())
                    .dateRegistration(new Date(System.currentTimeMillis()))
                    .id(membershipType.getId())
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
            MembershipType membershipType = membershipTypeRepository.save(MembershipType.builder()
                    .id(id)
                    .dateRegistration(new Date(System.currentTimeMillis()))
                    .name(name)
                    .status(true)
                    .build()
            );
            return membershipTypeMapper.INSTANCE.membershipTypeToMembershipTypeDTO(membershipType);
        }catch (RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }
}

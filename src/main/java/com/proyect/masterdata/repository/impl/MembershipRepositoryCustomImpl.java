package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.Membership;
import com.proyect.masterdata.repository.MembershipRepositoryCustom;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.*;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;

@Repository
public class MembershipRepositoryCustomImpl implements MembershipRepositoryCustom {
    @PersistenceContext(name="entityManager")
    private EntityManager entityManager;
    @Override
    public Page<Membership> searchForMembership(Long idMembership, Long idModule, String sort, String sortColumn, Integer pageNumber, Integer pageSize) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Membership> criteriaQuery = criteriaBuilder.createQuery(Membership.class);
        Root<Membership> itemRoot = criteriaQuery.from(Membership.class);

        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicateConditions(idMembership,idModule,criteriaBuilder,itemRoot);

        if(!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)){
            List<Order> membershipList = new ArrayList<>();
            if(sort.equalsIgnoreCase("ASC")){
                membershipList = listASC(sortColumn,criteriaBuilder,itemRoot);
            }
            if(sort.equalsIgnoreCase("DESC")){
                membershipList = listDESC(sortColumn,criteriaBuilder,itemRoot);
            }
            criteriaQuery.where(conditions.toArray(new Predicate[]{})).orderBy(membershipList);
        }else{
            criteriaQuery.where(conditions.toArray(new Predicate[]{}));
        }

        TypedQuery<Membership> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber*pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber,pageSize);
        long count = getOrderCount(idMembership,idModule);
        return new PageImpl<>(orderTypedQuery.getResultList(),pageable,count);
    }

    public List<Predicate> predicateConditions(
            Long idMembership,
            Long idModulo,
            CriteriaBuilder criteriaBuilder,
            Root<Membership> itemRoot
    ){
        List<Predicate> conditions = new ArrayList<>();

        if(idMembership!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(
                                    itemRoot.get("id"),idMembership)));
        }

        if(idModulo!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(
                                    itemRoot.get("idModule"),idModulo)));
        }

        return conditions;
    }

    List<Order> listASC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<Membership> itemRoot
    ){
        List<Order> membershipList = new ArrayList<>();
        if(sortColumn.equalsIgnoreCase("id")){
            membershipList.add(criteriaBuilder.asc(itemRoot.get("id")));
        }
        if(sortColumn.equalsIgnoreCase("idModule")){
            membershipList.add(criteriaBuilder.asc(itemRoot.get("idModule")));
        }
        return membershipList;
    }

    List<Order> listDESC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<Membership> itemRoot
    ){
        List<Order> membershipList = new ArrayList<>();
        if(sortColumn.equalsIgnoreCase("id")){
            membershipList.add(criteriaBuilder.desc(itemRoot.get("id")));
        }
        if(sortColumn.equalsIgnoreCase("idModule")){
            membershipList.add(criteriaBuilder.desc(itemRoot.get("idModule")));
        }
        return membershipList;
    }

    private long getOrderCount(Long idMembership,Long idModule){
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<Membership> itemRoot = criteriaQuery.from(Membership.class);

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(idMembership,idModule,criteriaBuilder,itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[]{}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}

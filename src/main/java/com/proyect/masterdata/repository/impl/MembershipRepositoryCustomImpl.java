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
import java.util.Date;
import java.util.List;

@Repository
public class MembershipRepositoryCustomImpl implements MembershipRepositoryCustom {
    @PersistenceContext(name="entityManager")
    private EntityManager entityManager;
    @Override
    public Page<Membership> searchForMembership(
            Long clientId, 
            Long membershipStateId,
            Long subscriptionId,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            String sort, 
            String sortColumn, 
            Integer pageNumber, 
            Integer pageSize,
            Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Membership> criteriaQuery = criteriaBuilder.createQuery(Membership.class);
        Root<Membership> itemRoot = criteriaQuery.from(Membership.class);

        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicateConditions(
                clientId,
                membershipStateId,
                subscriptionId,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                status,
                criteriaBuilder,
                itemRoot);

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
        long count = getOrderCount(
                clientId,
                membershipStateId,
                subscriptionId,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                status);
        return new PageImpl<>(orderTypedQuery.getResultList(),pageable,count);
    }

    public List<Predicate> predicateConditions(
            Long clientId,
            Long stateId,
            Long subscriptionId,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            Boolean status,
            CriteriaBuilder criteriaBuilder,
            Root<Membership> itemRoot
    ){
        List<Predicate> conditions = new ArrayList<>();

        if(clientId!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(
                                    itemRoot.get("clientId"),clientId)));
        }

        if(stateId!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(
                                    itemRoot.get("membershipStateId"),stateId)));
        }

        if(subscriptionId!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(
                                    itemRoot.get("subscriptionId"),subscriptionId)));
        }

        if(registrationStartDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.greaterThanOrEqualTo(itemRoot.get("registrationDate"),registrationStartDate)
                    )
            );
        }

        if(registrationEndDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.lessThanOrEqualTo(itemRoot.get("registrationDate"),registrationEndDate)
                    )
            );
        }

        if(updateStartDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.greaterThanOrEqualTo(itemRoot.get("updateDate"),updateStartDate)
                    )
            );
        }

        if(updateEndDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.lessThanOrEqualTo(itemRoot.get("updateDate"),updateEndDate)
                    )
            );
        }

        if (status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("status"))));
        }

        if (!status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("status"))));
        }

        return conditions;
    }
    List<Order> listASC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<Membership> itemRoot
    ){
        List<Order> membershipList = new ArrayList<>();
        if(sortColumn.equalsIgnoreCase("clientId")){
            membershipList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }
        if(sortColumn.equalsIgnoreCase("membershipStateId")){
            membershipList.add(criteriaBuilder.asc(itemRoot.get("membershipStateId")));
        }
        if(sortColumn.equalsIgnoreCase("subscriptionId")){
            membershipList.add(criteriaBuilder.asc(itemRoot.get("subscriptionId")));
        }
        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            membershipList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            membershipList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            membershipList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            membershipList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }
        return membershipList;
    }
    List<Order> listDESC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<Membership> itemRoot
    ){
        List<Order> membershipList = new ArrayList<>();
        if(sortColumn.equalsIgnoreCase("clientId")){
            membershipList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }
        if(sortColumn.equalsIgnoreCase("stateId")){
            membershipList.add(criteriaBuilder.desc(itemRoot.get("stateId")));
        }
        if(sortColumn.equalsIgnoreCase("subscriptionId")){
            membershipList.add(criteriaBuilder.desc(itemRoot.get("subscriptionId")));
        }
        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            membershipList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            membershipList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            membershipList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            membershipList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }
        return membershipList;
    }

    private long getOrderCount(
            Long clientId,
            Long stateId,
            Long subscriptionId,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            Boolean status){
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<Membership> itemRoot = criteriaQuery.from(Membership.class);

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(
                clientId,
                stateId,
                subscriptionId,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                status,
                criteriaBuilder,
                itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[]{}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}

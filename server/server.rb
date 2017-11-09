
require 'sinatra'
require 'sequel'

DB = Sequel.postgres(username: 'postgres')


get '/latest-vote' do
  latest_vote = DB[:votes].order(:date).last
  data_for_vote(latest_vote)
end

get '/vote/:id' do |id|
  vote = DB[:votes][{id: id}]
  data_for_vote(vote)
end

def data_for_vote(vote)
  votes = DB[:vote_events].where(
    vote_id: vote[:id]
  ).join(
    :people, person_id: :person_id
  ).to_a

  vote.merge(votes: votes).to_json
end
